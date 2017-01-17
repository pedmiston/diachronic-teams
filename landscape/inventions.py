from os import environ
from py2neo import Graph, Subgraph, Node, Relationship

import recipes


# Fill up items and relationships before creating them in the graph
items = {}
relationships = []


def make_invention_from_recipe(recipe):
    materials_labels, result_label = recipe.replace(' ', '').split('=')

    # Retrieve materials from items.
    # ASSUMES recipes are in order.
    materials = [items[material] for material in materials_labels.split('+')]

    # Create a new item for the result of the invention.
    generation = max([material['generation'] for material in materials]) + 1
    result = Node('Item', label=result_label, generation=generation)
    items[result_label] = result

    for material in materials:
        relationships.append(Relationship(result, 'REQUIRES', material))


if __name__ == '__main__':
    graph = Graph(password=environ.get('NEO4J_PASSWORD'))

    # Make resource nodes
    items.update({label: Node('Item', label=label, generation=1)
                  for label in recipes.initial_resources.values()})

    for recipe in recipes.make_recipes():
        make_invention_from_recipe(recipe)

    data = Subgraph(nodes=items.values(), relationships=relationships)
    graph.merge(data)
