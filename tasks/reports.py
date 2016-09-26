from glob import glob

from invoke import task, run
import unipath

from .paths import PROJ

@task
def render(ctx, names=None, clear_cache=False, output='all',
           directory='reports'):
    """Compile RMarkdown reports to their output formats."""
    rmds = _parse_names(directory, names)
    for rmd in rmds:
        if clear_cache:
            _clear_report_cache(rmd)
        cmd = 'Rscript -e "rmarkdown::render(\'{}\', \'{}\')"'
        run(cmd.format(rmd, output))


@task
def list_chunks(ctx, chunk_file):
    """Print the available chunks to use in an RMarkdown document"""
    chunks = [line.strip().split()[-1]
              for line in open(chunk_file, 'r').readlines()
              if line.startswith('# ---- ')]
    for chunk in chunks:
        print(chunk)


def _parse_names(directory, names=None):
    names = names or '*.Rmd'
    matcher = '{proj}/{directory}/**/{names}'
    return glob(matcher.format(proj=PROJ, directory=directory, names=names),
                recursive=True)


def _clear_report_cache(rmd):
    assert rmd.exists()
    cache_dir = unipath.Path(rmd.parent, '.cache')
    if cache_dir.isdir():
        cache_dir.rmtree()
