from invoke import Collection
from . import kaggle, evoteams, reports

namespace = Collection(kaggle, evoteams, reports)
