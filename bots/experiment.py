from collections import namedtuple
from itertools import prod
from . import teams


SimVars = namedtuple('SimVars', ['team', 'seed'])


def read_experiment_yaml(experiment):
    data = yaml.load(open(experiment))
    data['src'] = experiment
    return Experiment(data)


class Experiment:
    """An experiment is made up of a bunch of simulations."""
    def __init__(self, data):
        self.options = SimVars(data[k] for k in SimVars._fields)

    def simulations(self):
        for sim_vars in prod(*self.options):
            yield Simulation(sim_vars)


class Simulation:
    def __init__(self, sim_vars):
        self.vars = SimVars(*sim_vars)

    def run(self):
        team = teams.get(self.vars.team)
