from invoke import Collection
from . import totems, docs, landscapes, simulations, experiment

namespace = Collection(evoteams, docs, landscapes, simulations, totems)
