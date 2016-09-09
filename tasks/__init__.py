from invoke import Collection
from . import kaggle, evoteams

namespace = Collection(kaggle, evoteams)
