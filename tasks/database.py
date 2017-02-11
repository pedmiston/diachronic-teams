from invoke import task


@task
def install(ctx):
    """Install the totems database on a remote server."""
    ctx.run('cd database && ansible-playbook install.yml')


@task
def snapshot(ctx):
    """Take a snapshot of the totems database."""
    ctx.run('cd database && ansible-playbook snapshot.yml')


@task
def restore(ctx, dump):
    """Restore the totems database from a snapshot."""
    cmd = 'cd database && ansible-playbook restore.yml -e dump={dump}'
    ctx.run(cmd.format(dump=dump))
