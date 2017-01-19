from invoke import task
import landscape


@task
def import_graph(ctx):
    """Make the totems landscape as a graph database."""
    landscape.import_innovations()


@task
def tree(ctx):
    """Visualize the totems landscape in a figure."""
    ctx.run('cd landscape && python tree.py')
