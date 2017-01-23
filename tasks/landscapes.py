from invoke import task
import landscapes


@task
def load(ctx, delete_first=False):
    """Make the totems landscape as a graph database."""
    landscapes.load(delete_first=delete_first)


@task
def tree(ctx):
    """Visualize the totems landscape in a figure."""
    viz = landscapes.make_graphviz()
    viz.format = 'png'
    viz.render('landscape.gv', view=True)
