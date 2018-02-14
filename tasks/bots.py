import sys
import json
from pathlib import Path

from invoke import task
import pandas

import bots
import graphdb
from tasks.config import R_PKG


@task(help=dict(experiment="Name of experiment to run. Pass 'list' to list all experiments."))
def run(ctx, experiment, output_dir=None, verbose=False, analyze_after=False):
    """Simulate robotic players playing the totems game.

    Examples:

      $ inv bots.run list  # list all experiments
      $ inv bots.run all   # run all experiments

    """
    experiments = determine_experiments(experiment)
    for experiment_yaml in experiments:
        if output_dir is None:
            output_dir = R_PKG / 'data-raw/bots'
        if not output_dir.is_dir():
            output_dir.mkdir(parents=True)
        output = Path(output_dir, experiment_yaml.stem + '.csv')
        print('Running experiment { %s }' % experiment_yaml.stem)
        bots.run_experiment(experiment_yaml, output=output, verbose=verbose)

    if analyze_after:
        analyze(ctx, experiment)


@task
def analyze(ctx, experiment):
    """Analyze the results of the simulation."""
    experiments = determine_experiments(experiment)
    for experiment_yaml in experiments:
        adjacent(experiment_yaml.stem)
        difficulty(experiment_yaml.stem)


def determine_experiments(experiment):
    if experiment == 'list':
        print('Available experiments:')
        for experiment in bots.paths.EXPERIMENTS.listdir('*.yaml'):
            print(' - ' + experiment.stem)
        sys.exit()
    elif experiment == 'all':
        experiments = bots.paths.EXPERIMENTS.listdir('*.yaml')
    elif Path(experiment).exists():
        experiments = [Path(experiment)]
        output_dir = output_dir or Path(experiment).parent
    else:
        experiment = Path(bots.paths.EXPERIMENTS, experiment + '.yaml')
        assert experiment.exists(), 'experiment %s not found' % experiment
        experiments = [experiment]

    return experiments


def adjacent(inventories):
    """Determine the number of adjacent items."""
    inventories_csv = find_simulations_csv(inventories)
    results = pandas.read_csv(inventories_csv)
    inventories = results.inventory.apply(json.loads)

    landscape = graphdb.Landscape()
    results['n_adjacent'] = \
        (inventories.apply(landscape.adjacent_possible)
                    .apply(len))
    results.to_csv(inventories_csv, index=False)


def difficulty(inventories):
    inventories_csv = find_simulations_csv(inventories)
    results = pandas.read_csv(inventories_csv)
    results['difficulty'] = results.inventory_size/results.n_adjacent
    results.to_csv(inventories_csv, index=False)


def find_simulations_csv(inventories):
    return Path(R_PKG, 'data-raw/bots', inventories+'.csv')


# @task
def expand(ctx, experiment):
    """Show the simulation vars used in an experiment."""
    if not Path(experiment).exists():
        experiment = Path(bots.paths.EXPERIMENTS, experiment + '.yaml')
        assert experiment.exists(), 'experiment %s not found' % experiment
    experiment = bots.read_experiment_yaml(experiment)
    simulations = experiment.expand_all()
    simulations.to_csv(sys.stdout, index=False)
