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


viz = Digraph()

for generation, cur_items in items.groupby('generation'):
    for label in cur_items.label:
        viz.node(label)

    # Loop twice so that nodes are created in groups
    # and can be aligned in a single generation.

    for label in cur_items.label:
        edges_ending_at_label = edges.ix[edges.item_to == label]
        for edge in edges_ending_at_label.itertuples():
            viz.edge(edge.item_from, edge.item_to)

viz.render('items.gv')
