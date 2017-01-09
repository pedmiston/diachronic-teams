from collections import namedtuple
from itertools import prod


SimVars = namedtuple('SimVars', ['team'])

class Experiment:
    """An experiment is a bunch of simulations."""
    def simulations(self):
        options = dict(team=['diachronic', 'synchronic'])
        options = SimVars(options[k] for k in SimVars._fields)
        for sim_vars in prod(*options):
            yield Simulation(sim_vars)


class Simulation:
    def __init__(self, *sim_vars):
        self.vars = SimVars(*sim_vars)

    def run(self):
        pass

