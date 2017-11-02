import sys
from glob import glob

from invoke import task
from unipath import Path


PROJ = Path(__file__).absolute().parent
R_PKG = Path(PROJ, 'data')
REPORTS = Path(PROJ, 'reports')
TOTEMS = Path(PROJ, 'experiment')
ITEM_IMAGES = Path(R_PKG, 'inst/extdata/items')
TOTEMS_RAW_DATA = Path(R_PKG, 'data-raw/totems')


@task
def install(ctx):
    """Install the totems R package."""
    ctx.run('cd {} && Rscript -e "devtools::install()"'.format(R_PKG))

@task
def make(ctx, name, reset_before=False, open_after=False, verbose=False):
    """Compile RMarkdown reports to their output formats.

    Examples:

      $ inv docs.make list   # see available reports
      $ inv docs.make all    # run all reports
      $ inv docs.make totems # make the totems.Rmd

    """
    reports = reports_from_name(name)
    failed = []

    cmd = 'Rscript -e "rmarkdown::render({!r})"'
    for report in reports:
        if reset_before:
            reset(ctx, report, verbose=verbose)

        result = ctx.run(cmd.format(str(report)), echo=verbose, warn=True)

        if not result.ok:
            failed.append(str(report))

        if open_after and result.ok:
            output_file = Path(report.parent, '{}.html'.format(report.stem))
            ctx.run('open {}'.format(output_file), echo=verbose)

    print('The following reports had errors:')
    for report in failed:
        print(' - {}'.format(report))

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
    EXTDATA = Path(R_PKG, 'inst/extdata/')
    if name == 'list':
        print('\n'.join(EXTDATA.listdir('*.gv', names_only=True)))
        return
    src = Path(EXTDATA, '{}.gv'.format(name))
    dst = Path('img/{}.{}'.format(output or name, ext))
    ctx.run('dot -T{} -Gdpi={} -o {} {}'.format(ext, dpi, dst, src))


def reports_from_name(name):
    available_reports = [Path(rmd) for rmd in
                         glob('{proj}/docs/**/*.Rmd'.format(proj=PROJ),
                              recursive=True)
                         if Path(rmd).isfile()]

    if name == 'all':
        rmds = available_reports
    elif name == 'list':
        print('Available docs:')
        for rmd in available_reports:
            print(' - %s' % rmd.stem)
        sys.exit()
    elif Path(name).isfile():
        rmds = [Path(name)]
    else:
        rmds = [Path(rmd) for rmd in
                glob('{proj}/docs/**/{name}*.Rmd'.format(proj=PROJ, name=name),
                     recursive=True)
                if Path(rmd).isfile()]

    return rmds
