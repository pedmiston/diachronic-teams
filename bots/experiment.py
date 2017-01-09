from collections import namedtuple
from itertools import prod
from . import teams


SimVars = namedtuple('SimVars', 'team players guesses seed')


def simulate(strategy, players, guesses, seed):
    team = Team(strategy, players)
    for guess in range(guesses):



def read_experiment_yaml(experiment):
    data = yaml.load(open(experiment))
    data['src'] = experiment
    return Experiment(data)


class Experiment:
    """An experiment is made up of a bunch of simulations."""
    def __init__(self, data):
        self._data = data

    def simulations(self):
        options = [getattr(self, k) for k in SimVars._fields]
        for sim_vars in prod(*options):
            yield Simulation(sim_vars)

    @property
    def team(self):
        team_names = self.get_as_list('team')
        teams = [teams.get(name) for name in team_names]
        return teams

    @property
    def players(self):
        return self.get_as_list('players')

    @property
    def guesses(self):
        return self.get_as_list('guesses')

    @property
    def seed(self):
        return range(1, self._data.get('replicates')+1)

    def get_as_list(self, key, default=None):
        return get_as_list(self._data, key, default)


class Simulation:
    def __init__(self, sim_vars):
        self.vars = SimVars(*sim_vars)

    def run(self):
        team = self.vars.team

def simulate(sim_vars):
    team = sim_vars.team


def get_as_list(data, key, default=None):
    result = data.get(key, default)
    if not isinstance(result, list):
        result = [result]
    return result
