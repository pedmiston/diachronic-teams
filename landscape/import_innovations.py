#!/usr/bin/env python
from os import environ
from collections import namedtuple
from py2neo import Node, Relationship, Graph, Subgraph, Unauthorized
import pandas
import unipath


initial_resources = {1: 'Big_Tree', 2: 'Tree', 3: 'Stone',
                     4: 'Red_Berry', 5: 'Blue_Berry', 6: 'Antler'}
landscape_dir = unipath.Path(__file__).parent
answer_key_csv = unipath.Path(landscape_dir, 'answer_key.csv')
Recipe = namedtuple('Recipe', 'requirements result')


def Item(label, number):
    return Node('Item', label=label, number=number)


def connect_to_graph_db():
    if 'NEO4J_PASSWORD' not in environ:
        raise Unauthorized('must set NEO4J_PASSWORD env variable')
    return Graph(password=environ['NEO4J_PASSWORD'])


def import_innovations():
    nodes = []
    relationships = []

    answer_key = pandas.read_csv(answer_key_csv)
    labels = make_labels(answer_key)
    recipes = answer_key.apply(to_recipe, axis=1, labels=labels)
    for requirements, result in recipes:
        nodes.append(result)
        for requirement in requirements:
            nodes.append(requirement)
            relationships.append(Relationship(result, 'REQUIRES', requirement))

    graph = connect_to_graph_db()
    for node in nodes:
        graph.merge(node)
    for relationship in relationships:
        graph.merge(relationship)


def make_labels(answer_key):
    # Start with initial resources and update with innovations
    labels = initial_resources.copy()
    # Item labels are item names without image file extentions
    answer_key['Label'] = answer_key.Name.str.split('.').str[0]
    labels.update(answer_key.set_index('Number')['Label'].to_dict())
    return labels


def to_recipe(innovation, labels):
    requirements = [Item(label=labels[number], number=number) for number in
                    innovation[['Item1', 'Item2', 'Item3', 'Item4']].tolist()
                    if number != 0]
    result = Item(label=labels[innovation.Number], number=innovation.Number)
    return Recipe(requirements, result)


if __name__ == '__main__':
    import_innovations()
