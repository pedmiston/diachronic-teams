from invoke import task, run


@task
def use_data(ctx):
    """Compile data to .rda in evoteams R pkg."""
    run('cd evoteams && Rscript data-raw/use-data.R')


@task
def install(ctx):
    """Install the evoteams R pkg."""
    run('bin/install')
