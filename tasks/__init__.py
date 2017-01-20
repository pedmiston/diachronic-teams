from invoke import Collection
from . import evoteams, reports, docs, landscapes, bots, totems

namespace = Collection(evoteams, reports, docs, landscapes, bots, totems)
