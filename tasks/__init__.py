from invoke import Collection
from . import evoteams, reports, docs, landscape, bots

namespace = Collection(evoteams, reports, docs, landscape, bots)
