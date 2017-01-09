from invoke import Collection
from . import evoteams, reports, docs, landscape

namespace = Collection(evoteams, reports, docs, landscape)
