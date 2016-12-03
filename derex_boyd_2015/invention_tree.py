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

for n in items.label:
    viz.node(n)

for e in edges.itertuples():
    viz.edge(e.item_from, e.item_to)

viz.render('items.gv', view=True)
