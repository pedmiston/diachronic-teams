from invoke import task, run

from .paths import REPORTS


@task
def render(ctx, names=None):
    """Compile RMarkdown reports to their output formats."""
    rmds = _parse_names(names)
    for rmd in rmds:
        cmd = 'Rscript -e "rmarkdown::render(\'{}\')"'
        run(cmd.format(rmd))


@task
def list_chunks(ctx, chunk_file):
    """Print the available chunks to use in an RMarkdown document"""

    chunks = [line.strip().split()[-1]
              for line in open(chunk_file, 'r').readlines()
              if line.startswith('# ---- ')]
    for chunk in chunks:
        print(chunk)


def _parse_names(names=None):
    names = names or '*.Rmd'
    return list(REPORTS.walk(names))
