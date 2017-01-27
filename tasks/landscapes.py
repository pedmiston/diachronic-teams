from invoke import task
import landscapes
from .paths import Path, R_PKG, TOTEMS, ITEM_IMAGES


@task
def start(ctx):
    """Start the Neo4j server."""
    ctx.run('neo4j start')
    print("Don't forget to `source environment` to set the NEO4J_PASSWORD!")


@task
def load(ctx, delete_first=False, load_only=False):
    """Make the totems landscape as a graph database."""
    landscapes.load(delete_first=delete_first)


@task
def tree(ctx, max_number=None, max_generation=None, name=None, view_off=False):
    """Visualize the totems landscape in a figure."""
    viz = landscapes.make_graphviz(image_dir=ITEM_IMAGES,
                                   max_number=max_number,
                                   max_generation=max_generation)
    viz.format = 'png'
    name = name or 'landscape'
    output = Path(R_PKG, 'inst/extdata/', name+'.gv')
    viz.render(output, view=not view_off)
