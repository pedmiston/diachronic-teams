#!/usr/bin/env python
from os import environ
from py2neo import Graph
import pandas
from graphviz import Digraph


graph = Graph(password=environ.get('NEO4J_PASSWORD'))

items = pandas.DataFrame(graph.data("""
MATCH (n:item)
RETURN n.generation as generation, n.label as label
"""))

edges = pandas.DataFrame(graph.data("""
MATCH (material:item) -[r:REQUIRES]-> (result:item)
RETURN material.label as item_from, result.label as item_to
"""))


viz = Digraph(graph_attr=dict(rankdir='BT'))

for item in items.label:
    viz.node(item)

for edge in edges.itertuples():
    viz.edge(edge.item_to, edge.item_from)

# Set rank for nodes by generation.
# Insert { rank=same ... } calls for each generation into the dot source.
rank_fmt = '{{ rank=same {labels} }}\n'
for _, items_in_gen in items.groupby('generation'):
    spaced_labels = ' '.join(items_in_gen.label.tolist())
    viz.body.append(rank_fmt.format(labels=spaced_labels))

viz.render('items.gv', view=True)
