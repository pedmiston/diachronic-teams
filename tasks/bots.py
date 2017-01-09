import sys
import invoke
import bots


@invoke.task
def run(ctx, output=None):
    """Simulate robotic players playing the totems game."""
    experiment = bots.Experiment()
    output = output or sys.stdout
    for ix, simulation in enumerate(experiment.simulations()):
        results = simulation.run()
        results.insert(0, 'ix', ix)
        results.to_csv(output, mode='a', index=False)
