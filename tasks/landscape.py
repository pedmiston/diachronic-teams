from invoke import task


@task
def make(ctx):
    """Make the totems landscape as a graph database."""
    ctx.run('cd landscape && python inventions.py')


@task
def tree(ctx):
    """Visualize the totems landscape in a figure."""
    ctx.run('cd landscape && python tree.py')
