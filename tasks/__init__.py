from invoke import Collection
from . import evoteams, reports, docs, landscape, bots, totems

namespace = Collection(evoteams, reports, docs, landscape, bots, totems)
