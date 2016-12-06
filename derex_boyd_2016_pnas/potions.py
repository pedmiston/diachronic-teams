from os import environ
from py2neo import Graph, Subgraph, Node, Relationship


graph = Graph(password=environ.get('NEO4J_PASSWORD'))

# Fill up items and relationships before creating them in the graph
potions = {}
relationships = []

# Make resource nodes
labels = 'pink_beaker yellow_beaker purple_beaker red_bulb green_bulb yellow_bulb'.split()
potions.update({label: Node('Potion', label=label, generation=1)
               for label in labels})

recipes = """
yellow_beaker + red_bulb + yellow_bulb = brown_jar
brown_jar + yellow_beaker + purple_beaker = green_jar
green_jar + purple_beaker + green_bulb = green_flask
pink_beaker + purple_beaker + green_bulb = yellow_jar
yellow_jar + red_bulb + yellow_bulb = red_jar
red_jar + pink_beaker + red_bulb = blue_flask
green_flask + blue_flask + green_jar = orange_jar
blue_flask + green_flask + red_jar = gold_jar
""".strip().split('\n')


def make_potion_from_recipe(recipe):
    materials_labels, result_label = recipe.replace(' ', '').split('=')

    # Retrieve materials from items.
    # ASSUMES recipes are in order.
    materials = [potions[material] for material in materials_labels.split('+')]

    # Create a new item for the result of the invention.
    generation = max([material['generation'] for material in materials]) + 1
    result = Node('Potion', label=result_label, generation=generation)
    potions[result_label] = result

    for material in materials:
        relationships.append(Relationship(result, 'REQUIRES', material))


for recipe in recipes:
    make_potion_from_recipe(recipe)

data = Subgraph(nodes=potions.values(), relationships=relationships)
graph.create(data)
