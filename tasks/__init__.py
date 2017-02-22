from invoke import Collection
from . import r_pkg, docs, graph, bots, exps, db

namespace = Collection(r_pkg, docs, graph, bots, exps, db)
