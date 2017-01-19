from invoke import task
import landscape


@task
def load(ctx):
    """Make the totems landscape as a graph database."""
    landscape.load()


@task
def tree(ctx):
    """Visualize the totems landscape in a figure."""
    viz = landscape.make_graphviz()
    viz.format = 'png'
    viz.render('landscape.gv', view=True)
