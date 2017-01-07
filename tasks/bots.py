from invoke import task

import bots


@task
def run(ctx, experiment):
    """Simulate robotic players playing the totems game."""
    experiment = bots.read_experiment_yaml(experiment)
    for ix, simulation in enumerate(experiment.simulations()):
        results = simulation.run()
        results.insert(0, 'ix', ix)
        results.to_csv(experiment.output_filename, mode='a', index=False)
