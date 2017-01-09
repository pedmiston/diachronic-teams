from itertools import product

from . import paths
from .strategies import strategies
from .models import Team
from .landscapes import Landscape
from .util import get_as_list


"""An instance of SimVars defines the parameters for running a simulation.

The order of the fields in SimVars must match the call signature for the
"simulate" function. Also, all SimVars fields must be available as properties
of the Experiment class.
"""
SimVars = namedtuple('SimVars', 'strategy guesses n_players seed')


def simulate(strategy, n_guesses, n_players, seed):
    landscape = Landscape()

    for _ in strategy(team, n_guesses):
        guesses = team.guess()
        new_items = landscape.evaluate_guesses(guesses)
        if len(new_items) > 0:
            team.add_to_inventory(new_items)


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
