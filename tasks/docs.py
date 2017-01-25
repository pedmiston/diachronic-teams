from invoke import task
from unipath import Path

@task(help=dict(name='If name is "list", list available names.'))
def img(ctx, name, output=None, ext='png', dpi=300):
    if name == 'list':
        print('\n'.join(Path('evoteams/inst/extdata/').listdir()))
        return
    src = Path('evoteams/inst/extdata/{}.gv'.format(name))
    dst = Path('img/{}.{}'.format(output or name, ext))
    ctx.run('dot -T{} -Gdpi={} -o {} {}'.format(ext, dpi, dst, src))
