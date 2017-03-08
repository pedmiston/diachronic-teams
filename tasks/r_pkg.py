from invoke import task

import tasks
from tasks.paths import R_PKG, Path


@task
def use_data(ctx, use_data_script=None):
    """Compile data to .rda in totems R pkg."""
    if use_data_script is None:
        use_data_scripts = Path(R_PKG, 'data-raw/').listdir('use-data*.R')
    else:
        use_data_scripts = [Path(R_PKG, 'data-raw/', use_data_script + '.R')]

    cmd = 'cd {R_pkg} && Rscript {use_data_script}'
    for use_data_script in use_data_scripts:
        ctx.run(cmd.format(R_pkg=R_PKG, use_data_script=use_data_script),
                echo=True)


@task
def install(ctx, use_data_too=False, make_graph=False,
            use_data_script=None, document_only=False):
    """Install the totems R pkg."""
    cmd = 'cd {R_pkg} && Rscript -e "{R_cmds}"'
    R_cmds = """
    library(devtools)
    install_github('pedmiston/crotchet')
    document()
    install()
    """.split()

    if use_data_too or use_data_script:
        use_data(ctx, use_data_script=use_data_script)

    if make_graph:
        tasks.graph.tree(ctx, view_off=True)
        tasks.graph.tree(ctx, max_generation=4, name='landscape-sample',
                         view_off=True)
        tasks.graph.tree(ctx, max_number=100, name='landscape-tools',
                         view_off=True)

    if document_only:
        R_cmds = ["devtools::document()"]

    ctx.run(cmd.format(R_pkg=R_PKG, R_cmds=';'.join(R_cmds)))