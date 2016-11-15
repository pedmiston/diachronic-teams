from glob import glob

from invoke import task, run
from unipath import Path

from .paths import PROJ


@task
def render(ctx, names=None, cache_clear=False, figs_clear=False, output='all',
           directory='reports', open_on_exit=False, report_ext='html'):
    """Compile RMarkdown reports to their output formats."""
    cmd = 'Rscript -e "rmarkdown::render(\'{rmd}\', \'{output_dir}\')"'
    rmds = _parse_names(directory, names)
    for rmd in rmds:
        if cache_clear:
            _clear_report_cache(rmd)
        if figs_clear:
            _clear_report_figs(rmd)
        run(cmd.format(rmd=rmd, output_dir=output))

        if open_on_exit:
            output_file = Path(rmd.parent, '{}.{}'.format(rmd.stem, report_ext))
            run('open {}'.format(output_file))


@task
def list_chunks(ctx, chunk_file):
    """Print the available chunks to use in an RMarkdown document"""
    if not Path(chunk_file).exists():
        chunk_file = Path('reports/chunks', chunk_file)
        if not chunk_file.exists():
            raise AssertionError('chunk file {} not found'.format(chunk_file))

    chunks = [line.strip().split()[-1]
              for line in open(chunk_file, 'r').readlines()
              if line.startswith('# ---- ')]

    for chunk in chunks:
        print(chunk)


def _parse_names(directory, names=None):
    names = names or '*.Rmd'
    matcher = '{proj}/{directory}/**/{names}'.format(
        proj=PROJ, directory=directory, names=names
    )
    return [Path(rmd) for rmd in glob(matcher, recursive=True)]


def _clear_report_cache(rmd):
    assert rmd.exists()
    cache_dir = Path(rmd.parent, '.cache')
    if cache_dir.isdir():
        cache_dir.rmtree()

def _clear_report_figs(rmd):
    assert rmd.exists()
    figs_dir = Path(rmd.parent, 'figs')
    if figs_dir.isdir():
        figs_dir.rmtree()
