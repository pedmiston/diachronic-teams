from os import environ
from py2neo import Graph, Subgraph, Node, Relationship


graph = Graph(password=environ.get('NEO4J_PASSWORD'))

if 'label' not in graph.schema.get_uniqueness_constraints('Potion'):
    graph.schema.create_uniqueness_constraint('Potion', 'label')

# Fill up items and relationships before creating them in the graph
potions = {}
relationships = []

# Make resource nodes
labels = 'pink_beaker orange_beaker purple_beaker red_bulb green_bulb yellow_bulb'.split()
potions.update({label: Node('Potion', label=label, generation=1)
               for label in labels})

recipes = """
orange_beaker + red_bulb + yellow_bulb = purple_jar
purple_jar + orange_beaker + purple_beaker = blue_jar
blue_jar + purple_beaker + green_bulb = green_flask
pink_beaker + purple_beaker + green_bulb = yellow_jar
yellow_jar + red_bulb + yellow_bulb = pink_jar
pink_jar + pink_beaker + red_bulb = black_flask
green_flask + black_flask + blue_jar = red_jar
black_flask + green_flask + pink_jar = rose_jar
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
graph.merge(data)
