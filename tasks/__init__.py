from invoke import Collection
from . import totems, docs, landscapes, simulations, experiment, database

namespace = Collection(totems, docs, landscapes, simulations, experiment, database)
