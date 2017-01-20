from invoke import task
import landscapes


@task
def load(ctx):
    """Make the totems landscape as a graph database."""
    landscapes.load()


@task
def tree(ctx):
    """Visualize the totems landscape in a figure."""
    viz = landscapes.make_graphviz()
    viz.format = 'png'
    viz.render('landscape.gv', view=True)
