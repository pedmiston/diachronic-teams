#!/usr/bin/env python
from py2neo import Node, Relationship, Graph, Subgraph
import pandas
import unipath

from landscapes.graph_db import connect_to_graph_db
from landscapes.util import max_generation, MissingGeneration


# Where to get the answer key
landscape_dir = unipath.Path(__file__).parent
answer_key_csv = unipath.Path(landscape_dir, 'answer_key.csv')

# These were not in the answer key
initial_resources = {1: 'Big_Tree', 2: 'Tree', 3: 'Stone',
                     4: 'Red_Berry', 5: 'Blue_Berry', 6: 'Antler'}


def load(delete_first=False):
    """Load item and recipe nodes and all relationships into Neo4j.

    Assumes the answer key is in order!
    """
    # Create item nodes for all initial resources
    items = {number: Item(number=number, label=label, generation=0)
             for number, label in initial_resources.items()}
    recipes = []
    relationships = []

    # Turn each row of the answer key into item and recipe
    # nodes and relationships.
    answer_key = pandas.read_csv(answer_key_csv)

    # Remove the image extension from item labels
    answer_key['Label'] = answer_key.Name.str.split('.').str[0]

    for data in answer_key.itertuples():
        try:
            requirements = [items[getattr(data, item)]
                            for item in 'Item1 Item2 Item3 Item4'.split()
                            if getattr(data, item) != 0]
        except KeyError:
            raise AnswerKeyOutOfOrder()

        try:
            max_generation(requirements)
        except MissingGeneration:
            raise AnswerKeyOutOfOrder()

        # Convert numpy.int64 to native python int because
        # py2neo nodes don't like numpy.int64 properties.
        number = data.Number.item()

        result = Item(number=number, label=data.Label,
                      generation=max_generation(requirements)+1)
        items[data.Number] = result

        recipe = Recipe(data.Code)
        recipes.append(recipe)

        relationships.append(Creates(recipe, result))
        for requirement in requirements:
            relationships.append(Requires(recipe, requirement))
            relationships.append(Inherits(result, requirement))

    graph = connect_to_graph_db()

    if delete_first:
        graph.run("MATCH (n:Item) DETACH DELETE n;")

    # Create nodes and relationships.
    #
    # Smells slow!
    # Looping is faster than one big subgraph.
    for item in items.values():
        graph.merge(item)
    for recipe in recipes:
        graph.merge(recipe)
    for relationship in relationships:
        graph.merge(relationship)


# Node and Relationship definitions

def Item(number, label, generation):
    assert isinstance(number, int), 'py2neo only likes native python ints'
    return Node('Item', number=number, label=label, generation=generation)

def Recipe(code):
    return Node('Recipe', code=code)

def Creates(recipe, result):
    return Relationship(recipe, 'CREATES', result)

def Requires(recipe, requirement):
    return Relationship(recipe, 'REQUIRES', requirement)

def Inherits(result, requirement):
    return Relationship(result, 'INHERITS', requirement)


class AnswerKeyOutOfOrder(Exception):
    """You're f----ed."""


if __name__ == '__main__':
    load()
