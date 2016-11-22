from invoke import task, run

from .paths import R_PKG


@task
def use_data(ctx):
    """Compile data to .rda in evoteams R pkg."""
    cmd = 'cd {R_pkg} && Rscript data-raw/use-data.R'
    run(cmd.format(R_pkg=R_PKG))


@task
def install(ctx, document_only=False):
    """Install the evoteams R pkg."""
    cmd = 'cd {R_pkg} && Rscript -e "{R_cmds}"'
    R_cmds = """
    library(devtools)
    install_github('pedmiston/crotchet')
    document()
    install()
    """.split()

    if document_only:
        R_cmds = ["devtools::document()"]

    run(cmd.format(R_pkg=R_PKG, R_cmds=';'.join(R_cmds)))
