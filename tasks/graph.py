from invoke import task
import json
import pandas

import graph
from tasks.paths import Path, R_PKG, TOTEMS, ITEM_IMAGES


@task
def load(ctx, delete_first=False, load_only=False):
    """Make the totems landscape as a graph database."""
    graph.load(delete_first=delete_first)


@task
def tree(ctx, max_number=None, max_generation=None, name=None, view_off=False):
    """Visualize the totems landscape in a figure.

    Examples:

        $ inv graph.tree
        $ inv graph.tree -n landscape-sample --max-generation 6
        $ inv graph.tree -n landscape-tools --max-number 100
    """
    viz = graph.make_graphviz(image_dir=ITEM_IMAGES,
                              max_number=max_number,
                              max_generation=max_generation)
    viz.format = 'png'
    name = name or 'landscape'
    output = Path(R_PKG, 'inst/extdata/', name+'.gv')
    viz.render(output, view=not view_off)
