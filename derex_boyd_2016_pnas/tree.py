#!/usr/bin/env python
from os import environ
from py2neo import Graph
import pandas
from graphviz import Digraph


graph = Graph(password=environ.get('NEO4J_PASSWORD'))

potions = pandas.DataFrame(graph.data("""
MATCH (n:Potion)
RETURN n.generation as generation, n.label as label
"""))

edges = pandas.DataFrame(graph.data("""
MATCH (material:Potion) -[r:REQUIRES]-> (result:Potion)
RETURN material.label as potion_from, result.label as potion_to
"""))

viz = Digraph(graph_attr=dict(rankdir='BT'),
              node_attr=dict(fontname='Helvetica', fontsize='12',
                             shape='none'))

for potion in potions.itertuples():
    viz.node(potion.label)

for edge in edges.itertuples():
    viz.edge(edge.potion_to, edge.potion_from)

# Set rank for nodes by generation.
# Insert { rank=same ... } calls for each generation into the dot source.
rank_fmt = '{{ rank=same {labels} }}\n'
for _, potions_in_gen in potions.groupby('generation'):
    spaced_labels = ' '.join(potions_in_gen.label.tolist())
    viz.body.append(rank_fmt.format(labels=spaced_labels))

viz.render('potions.gv', view=True)
