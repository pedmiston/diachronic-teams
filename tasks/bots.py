import sys
import json

import invoke
import pandas
from unipath import Path

import bots
import landscapes
from tasks.paths import R_PKG


@invoke.task
def run(ctx, experiment, output_dir=None, verbose=False, post_processing=False):
    """Simulate robotic players playing the totems game."""
    if experiment == 'list':
        print('Available experiments:')
        for experiment in bots.paths.EXPERIMENTS.listdir('*.yaml'):
            print(' - ' + experiment.stem)
        return
    elif experiment == 'all':
        experiments = bots.paths.EXPERIMENTS.listdir('*.yaml')
    elif Path(experiment).exists():
        experiments = [Path(experiment)]
        output_dir = output_dir or Path(experiment).parent
    else:
        experiment = Path(bots.paths.EXPERIMENTS, experiment + '.yaml')
        assert experiment.exists(), 'experiment %s not found' % experiment
        experiments = [experiment]

    for experiment_yaml in experiments:
        output_dir = Path(output_dir or Path(R_PKG, 'data-raw/bots'))
        if not output_dir.isdir():
            output_dir.mkdir(True)
        output = Path(output_dir, experiment_yaml.stem + '.csv')
        print('Running experiment { %s }' % experiment_yaml.stem)
        bots.run_experiment(experiment_yaml, output=output, verbose=verbose)

        if post_processing:
            adjacent(ctx, output)


@invoke.task
def expand(ctx, experiment):
    """Show the simulation vars used in an experiment."""
    if not Path(experiment).exists():
        experiment = Path(bots.paths.EXPERIMENTS, experiment + '.yaml')
        assert experiment.exists(), 'experiment %s not found' % experiment
    experiment = bots.read_experiment_yaml(experiment)
    simulations = experiment.expand_all()
    simulations.to_csv(sys.stdout, index=False)


@invoke.task
def adjacent(ctx, inventories, suffix=None):
    """Determine the number of adjacent items."""
    landscape = landscapes.Landscape()

    inventories_csv = find_bots_csv(inventories)
    results = pandas.read_csv(inventories_csv)
    inventories = results.inventory.apply(json.loads)
    results['n_adjacent'] = \
        (inventories.apply(landscape.determine_adjacent_possible)
                    .apply(len))
    if suffix:
        inventories_csv = find_bots_csv('{}-{}'.format(inventories, suffix))
    results.to_csv(inventories_csv, index=False)


def find_bots_csv(inventories):
    return Path(R_PKG, 'data-raw/bots', inventories+'.csv')
