def read_experiment_yaml(experiment):
    assert Path(experiment).exists(), "couldn't find experiment yaml"
    try:
        data = yaml.load(open(experiment))
    except Exception as err:
        raise ImproperExperimentConfig('file %s not proper yaml' % experiment)
    data['src'] = experiment
    return Experiment(data)


class Experiment:
    """An experiment is a bunch of simulations."""
    def __init__(self, data):
        self._data = data

    @property
    def output_filename(self):
        # 'exps/1.yaml' --> 'exps/1.csv'
        experiment = Path(self._data['src'])
        return Path(experiment.parent, experiment.stem+'.csv')

    def simulations(self):
        for sim_vars in self._sim_vars():
            yield Simulation(sim_vars)

    def _sim_vars(self):
        pass


class ImproperExperimentConfig(Exception):
    """The experiment config file was not formatted properly."""
