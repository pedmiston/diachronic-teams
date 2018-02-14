import sys
from pathlib import Path

import jinja2
from invoke import task, Collection

import graphdb

from tasks import R, docs, bots
from tasks.config import R_PKG


@task
def configure(ctx):
    """Create environment file from a template."""
    dst = '.environment'
    template = jinja2.Template(open('environment.j2', 'r').read())
    with open(dst, 'w') as f:
        f.write(template.render())


@task
def load(ctx):
    """Load the Neo4j graph db with totems data."""
    graphdb.load(delete_first=True)


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



namespace = Collection()
namespace.add_task(configure)
namespace.add_collection(R)
namespace.add_collection(docs)
namespace.add_collection(bots)
