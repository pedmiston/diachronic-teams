from invoke import task
from unipath import Path

@task
def img(ctx, name, ext='png'):
    if name == 'list':
        print('\n'.join(Path('evoteams/inst/extdata/').listdir()))
        return
    src = Path('evoteams/inst/extdata/{}.gv'.format(name))
    dst = Path('img/{}.{}'.format(name, ext))
    ctx.run('dot -T{} -o {} {}'.format(ext, dst, src))
