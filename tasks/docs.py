from invoke import task
from unipath import Path

@task
def img(ctx, name):
    if name == 'list':
        print('\n'.join(Path('evoteams/inst/extdata/').listdir()))
        return
    src = Path('evoteams/inst/extdata/{}.gv'.format(name))
    dst = Path('img/{}.png'.format(name))
    ctx.run('dot -Tpng -o {} {}'.format(dst, src), echo=True)
