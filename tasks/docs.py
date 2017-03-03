import sys
from glob import glob

from invoke import task
from unipath import Path

from .paths import PROJ


@task
def make(ctx, name, reset_before=False, open_after=False, verbose=False):
    """Compile RMarkdown reports to their output formats."""
    reports = reports_from_name(name)

    cmd = 'Rscript -e "rmarkdown::render({!r})"'
    for report in reports:
        if reset_before:
            reset(ctx, report, verbose=verbose)

        ctx.run(cmd.format(str(report)), echo=verbose)

        if open_after:
            output_file = Path(report.parent, '{}.html'.format(report.stem))
            ctx.run('open {}'.format(output_file), echo=verbose)


@task
def reset(ctx, name, verbose=False):
    """Clear the cache and outputs of RMarkdown reports."""
    reports = reports_from_name(name)

    for report in reports:
        cache_dir = Path(report.parent, '.cache.{}/'.format(report.stem))
        if cache_dir.isdir():
            cache_dir.rmtree()

        figs_dir = Path(report.parent, '{}-figs'.format(report.stem))
        if figs_dir.isdir():
            figs_dir.rmtree()

        code_str = Path(report.parent, 'code*')
        ctx.run('rm -rf {} {} {}'.format(cache_dir, figs_dir, code_str),
                echo=verbose)


@task(help=dict(name='If name is "list", list available figure names.'))
def img(ctx, name, output=None, ext='png', dpi=300):
    """Create an image and put it in the "img/" dir."""
    if name == 'list':
        print('\n'.join(Path('evoteams/inst/extdata/').listdir()))
        return
    src = Path('evoteams/inst/extdata/{}.gv'.format(name))
    dst = Path('img/{}.{}'.format(output or name, ext))
    ctx.run('dot -T{} -Gdpi={} -o {} {}'.format(ext, dpi, dst, src))


def reports_from_name(name):
    available_reports = [Path(rmd) for rmd in
                         glob('{proj}/docs/*.Rmd'.format(proj=PROJ))]
    if name == 'all':
        rmds = available_reports
    elif name == 'list':
        print('Available reports:')
        for rmd in available_reports:
            print(' - %s' % rmd.stem)
        sys.exit()
    elif Path(name).isfile():
        rmds = [Path(name)]
    else:
        rmds = [Path(rmd) for rmd in
                glob('{proj}/docs/{name}*.Rmd'.format(proj=PROJ, name=name))]

    return rmds
