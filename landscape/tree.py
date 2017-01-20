#!/usr/bin/env python
from os import environ
from py2neo import Graph
import pandas
from graphviz import Digraph
import unipath

from .graph_db import connect_to_graph_db

landscape_dir = unipath.Path(__file__).parent
images_dir = unipath.Path(landscape_dir, 'images')


def make_graphviz():
    graph = connect_to_graph_db()

    items = pandas.DataFrame(graph.data("""
    MATCH (n:Item)
    WHERE n.number < 100
    RETURN n.generation as generation, n.label as label
    """))

    edges = pandas.DataFrame(graph.data("""
    MATCH (material:Item) -[r:REQUIRES]-> (result:Item)
    WHERE material.number < 100 AND result.number < 100
    RETURN material.label as item_from, result.label as item_to
    """))

    viz = Digraph(graph_attr=dict(rankdir='TB'),
                  node_attr=dict(fontname='Helvetica', fontsize='12',
                                 shape='none'))

    for item in items.itertuples():
        viz.node(item.label, label='', image=to_image_path(item.label))

    for edge in edges.itertuples():
        viz.edge(edge.item_to, edge.item_from)

    # Set rank for nodes by generation.
    # Insert { rank=same ... } calls for each generation into the dot source.
    rank_fmt = '{{ rank=same {labels} }}\n'
    for _, items_in_gen in items.groupby('generation'):
        spaced_labels = ' '.join(items_in_gen.label.tolist())
        viz.body.append(rank_fmt.format(labels=spaced_labels))

    return viz


def to_image_path(stem):
    return unipath.Path(images_dir, stem + '.jpg')
