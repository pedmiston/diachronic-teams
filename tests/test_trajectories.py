import pandas
from tasks.exps import extract_trajectories


def test_trajectories():
    workshop = pandas.DataFrame({
        'Inventory': [['a', 'b', 'c'], ['a', 'b'], ['a']],
        'TrialTime': [3, 2, 1],
        'ID_Player': 1,
    })
    trajectories = extract_trajectories(workshop)
    assert len(trajectories.Trajectory.unique()) == 1
    assert trajectories.Trajectory.iloc[0] == 'a|a-b|a-b-c'
