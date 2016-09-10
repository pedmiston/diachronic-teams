from invoke import task, run

from .paths import REPORTS


@task
def render(ctx, names=None):
    """Compile RMarkdown reports to their output formats."""
    rmds = _parse_names(names)
    for rmd in rmds:
        cmd = 'Rscript -e "rmarkdown::render(\'{}\')"'
        run(cmd.format(rmd))


def _parse_names(names=None):
    names = names or '*.Rmd'
    return list(REPORTS.walk(names))
