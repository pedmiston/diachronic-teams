from sys import stdout
from itertools import product
from collections import namedtuple
import json

import yaml
import pandas

from .models import create_team
from .strategies import strategies
from .landscapes import Landscape
from .util import get_as_list


"""An instance of SimVars contains the parameters for running a simulation.

The order of the fields in SimVars must match the call signature for the
"simulate" function. Also, all SimVars fields must be available as properties
of the Experiment class.
"""
SimVars = namedtuple('SimVars', 'strategy n_guesses n_players seed')
RoundVars = namedtuple('RoundVars', 'guesses new_items inventory')


def simulate(strategy, n_guesses, n_players, seed):
    landscape = Landscape()
    team = create_team(n_players)
    rounds = []

    for iteration in strategy(team, n_guesses):
        guesses = team.make_guesses()
        new_items = landscape.evaluate_guesses(guesses)
        if len(new_items) > 0:
            team.inventory.extend(new_items)

        rounds.append(dict(
            strategy=strategy.__name__,
            n_guesses=n_guesses,
            n_players=n_players,
            seed=seed,
            round=iteration,
            guesses=json.dumps(guesses),
            new_items=json.dumps(new_items),
            inventory=json.dumps(team.inventory),
        ))

    output_cols = SimVars._fields + ['round'] + RoundVars._fields
    results = pandas.DataFrame.from_records(rounds, columns=output_cols)
    return results


def run_experiment(experiment_yaml, output=None):
    """Run an experiment, which is a collection of simulations."""
    experiment = read_experiment_yaml(experiment_yaml)
    output = open(output, 'w') if output else stdout
    for sim_id, sim_vars in enumerate(experiment.simulations()):
        results = simulate(*sim_vars)
        results.insert(0, 'sim_id', sim_id)
        first_write = (sim_id == 0)
        results.to_csv(output, index=False, header=first_write, mode='a')
    output.close()


def read_experiment_yaml(experiment_yaml):
    return Experiment.from_yaml(experiment_yaml)


class Experiment:
    """A group of simulations created from experimental variables."""
    def __init__(self, data=None):
        self._data = data or dict()

    @classmethod
    def from_yaml(cls, experiment_yaml):
        data = yaml.load(open(experiment_yaml))
        return cls(data)

    def simulations(self):
        """Yield the product of experiment variables."""
        props = [getattr(self, prop) for prop in SimVars._fields]
        return product(*props)

    def expand_all(self):
        return pandas.DataFrame(list(self.simulations()),
                                columns=SimVars._fields)

    def get_as_list(self, key, default=None):
        return get_as_list(self._data, key, default)

    # ---- Start of experiment variables as object properties ----

    @property
    def strategy(self):
        """A list of strategies."""
        names = self.get_as_list('strategy')
        return [strategies[name] for name in names]

    @property
    def n_guesses(self):
        """A list of guess amounts to be allotted to teams."""
        return self.get_as_list('guesses')

    @property
    def n_players(self):
        return self._data['n_players']

    @property
    def seed(self):
        """A list of seeds to use when initializing the teams."""
        return range(self._data['n_seeds'])
