from invoke import Collection
from . import kaggle, evoteams, reports, docs

namespace = Collection(kaggle, evoteams, reports, docs)
