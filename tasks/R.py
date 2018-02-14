from invoke import task

from tasks.config import R_PKG

@task
def install(ctx, verbose=False):
    """Install the totems R package."""
    ctx.run('cd {} && Rscript -e "devtools::install()"'.format(R_PKG), echo=verbose)

@task
def make(ctx, verbose=False):
    """Compile the .rda data files in the totems R package."""
    ctx.run('cd {} && Rscript make-data.R'.format(R_PKG), echo=verbose)
