from invoke import task


@task
def snapshot(ctx):
    """Take a snapshot of the totems database."""
    ctx.run('cd database && ansible-playbook download_db_dump.yml')
