from invoke import Collection
from . import evoteams, reports, docs

namespace = Collection(evoteams, reports, docs)
