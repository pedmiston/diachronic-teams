import simulations


def test_team_initial_inventory():
    team = bots.models.create_team()
    assert len(team.inventory) == 6
    assert set(team.inventory) == {'Big_Tree', 'Tree', 'Stone',
                                   'Red_Berry', 'Blue_Berry', 'Antler'}

def test_experiment_default_args():
    exp = bots.main.Experiment()
    assert exp.player_memory == [False]
    assert exp.team_memory == [True]
