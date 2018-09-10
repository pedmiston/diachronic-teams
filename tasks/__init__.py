import sys
from os import environ
from pathlib import Path

import jinja2
from invoke import task, Collection

import graphdb

from tasks import R, bots
from tasks.config import R_PKG


template = jinja2.Template(
    """\
export NEO4J_PASSWORD={{ neo4j_password }}
export ANSIBLE_VAULT_PASSWORD_FILE={{ password_file }}
"""
)


@task
def configure(ctx):
    """Create environment file from a template."""
    dst = ".env"

    neo4j_password = environ.get("NEO4J_PASSWORD", input("Enter Neo4j password: "))
    password_file = environ.get(
        "ANSIBLE_VAULT_PASSWORD_FILE", input("Enter path to password file: ")
    )

    with open(dst, "w") as f:
        f.write(
            template.render(neo4j_password=neo4j_password, password_file=password_file)
        )


@task
def load(ctx):
    """Load the Neo4j graph db with totems data."""
    graphdb.load(delete_first=True)


@task(help=dict(name='If name is "list", list available figure names.'))
def img(ctx, name, output=None, ext="png", dpi=300):
    """Create an image and put it in the "img/" dir."""
    EXTDATA = Path(R_PKG, "inst/extdata/")
    if name == "list":
        print("\n".join(EXTDATA.listdir("*.gv", names_only=True)))
        return
    src = Path(EXTDATA, "{}.gv".format(name))
    dst = Path("img/{}.{}".format(output or name, ext))
    ctx.run("dot -T{} -Gdpi={} -o {} {}".format(ext, dpi, dst, src))


namespace = Collection()
namespace.add_task(configure)
namespace.add_collection(R)
namespace.add_collection(bots)
