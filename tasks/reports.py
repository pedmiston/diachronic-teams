from glob import glob

from invoke import task, run
from unipath import Path

from .paths import PROJ


@task
def render(ctx, name, clear=False, open_after=False):
    """Compile RMarkdown reports to their output formats."""
    options = [Path(rmd) for rmd in
               glob('{proj}/reports/*.Rmd'.format(proj=PROJ))]
    if name == '*':
        rmds = options
    elif name == '?':
        print('Available reports:')
        for rmd in options:
            print(' - %s' % rmd.stem)
        return
    elif Path(name).exists():
        rmds = [Path(name)]
    else:
        rmds = [Path(rmd) for rmd in
                glob('{proj}/reports/{name}*.Rmd'.format(proj=PROJ, name=name))]

    cmd = 'Rscript -e "rmarkdown::render({!r})"'
    for rmd in rmds:
        if clear:
            _clear_report_cache(rmd)
            _clear_report_figs(rmd)

        run(cmd.format(str(rmd)))

        if open_after:
            output_file = Path(rmd.parent, '{}.html'.format(rmd.stem))
            run('open {}'.format(output_file))

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
