from glob import glob

from invoke import task
from unipath import Path

from .paths import PROJ


@task
def make(ctx, name, clear=False, open_after=False):
    """Compile RMarkdown reports to their output formats."""
    available_reports = [Path(rmd) for rmd in
                         glob('{proj}/docs/*.Rmd'.format(proj=PROJ))]
    if name == 'all':
        rmds = available_reports
    elif name == 'list':
        print('Available reports:')
        for rmd in available_reports:
            print(' - %s' % rmd.stem)
        return
    elif Path(name).exists():
        rmds = [Path(name)]
    else:
        rmds = [Path(rmd) for rmd in
                glob('{proj}/docs/{name}*.Rmd'.format(proj=PROJ, name=name))]

    cmd = 'Rscript -e "rmarkdown::render({!r})"'
    for rmd in rmds:
        if clear:
            cache_dir = Path(rmd.parent, '.cache')
            if cache_dir.isdir():
                cache_dir.rmtree()

            figs_dir = Path(rmd.parent, 'figs')
            if figs_dir.isdir():
                figs_dir.rmtree()

            ctx.run('rm -f {}/code*'.format(rmd.parent))

        ctx.run(cmd.format(str(rmd)))

        if open_after:
            output_file = Path(rmd.parent, '{}.html'.format(rmd.stem))
            ctx.run('open {}'.format(output_file))


@task(help=dict(name='If name is "list", list available names.'))
def img(ctx, name, output=None, ext='png', dpi=300):
    """Create an image and put it in the "img/" dir."""
    if name == 'list':
        print('\n'.join(Path('evoteams/inst/extdata/').listdir()))
        return
    src = Path('evoteams/inst/extdata/{}.gv'.format(name))
    dst = Path('img/{}.{}'.format(output or name, ext))
    ctx.run('dot -T{} -Gdpi={} -o {} {}'.format(ext, dpi, dst, src))
