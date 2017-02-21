from invoke import Collection
from . import totems, docs, landscapes, bots, exps, database

namespace = Collection(totems, docs, landscapes, bots, exps, database)
