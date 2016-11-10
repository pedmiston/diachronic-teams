from invoke import task
from unipath import Path


@task
def build(ctx, name, open=True):
    """Build the named document."""
    dir = Path('docs', name)
    flags = '-o' if open else ''
    cmd = ctx.run('cd {} && ./build {}'.format(dir, flags))
