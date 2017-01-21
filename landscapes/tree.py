import pandas
from graphviz import Digraph
import unipath

from landscapes.graph_db import connect_to_graph_db
from landscapes.util import path_to_image


def make_graphviz():
    graph = connect_to_graph_db()

    items = pandas.DataFrame(graph.data("""
    MATCH (n:Item)
    WHERE n.number < 100
    RETURN n.generation as generation, n.label as label
    """))

    edges = pandas.DataFrame(graph.data("""
    MATCH (result:Item) -[r:INHERITS]-> (requirement:Item)
    WHERE result.number < 100 AND requirement.number < 100
    RETURN result.label as result, requirement.label as requirement
    """))

    viz = Digraph(graph_attr=dict(rankdir='TB'),
                  node_attr=dict(fontname='Helvetica', fontsize='12',
                                 shape='none'))

    for item in items.itertuples():
        viz.node(item.label, label='', image=path_to_image(item.label))

    for edge in edges.itertuples():
        viz.edge(edge.requirement, edge.result)

    # Set rank for nodes by generation.
    # Insert { rank=same ... } calls for each generation into the dot source.
    rank_fmt = '{{ rank=same {labels} }}\n'
    for _, items_in_gen in items.groupby('generation'):
        spaced_labels = ' '.join(items_in_gen.label.tolist())
        viz.body.append(rank_fmt.format(labels=spaced_labels))

    return viz
