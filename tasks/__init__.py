from invoke import Collection
from . import evoteams, docs, landscapes, bots, totems

namespace = Collection(evoteams, docs, landscapes, bots, totems)
