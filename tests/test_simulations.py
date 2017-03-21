import json
import simulations
from numpy import int64


def test_team_initial_inventory():
    team = simulations.models.create_team()
    assert len(team.inventory) == 6
    assert set(team.inventory) == {1, 2, 3, 4, 5, 6}

def test_experiment_default_args():
    exp = simulations.main.Experiment()
    assert exp.player_memory == [False]
    assert exp.team_memory == [True]

def test_jsonify_new_items():
    new_items = {frozenset(["Stone", "Antler"]): "Antler_Refined"}
    expected = [{"guess": ["Antler", "Stone"], "result": "Antler_Refined"}]
    result_json = simulations.util.jsonify_new_items(new_items)
    result = json.loads(result_json)
    assert result[0]['result'] == expected[0]['result']
    assert set(result[0]['guess']) == set(expected[0]['guess'])

def test_jsonify_new_item_numbers():
    new_items = {frozenset([3, 6]): 23}
    expected = [{"guess": [3, 6], "result": 23}]
    result_json = simulations.util.jsonify_new_items(new_items)
    result = json.loads(result_json)
    assert result[0]['result'] == expected[0]['result']
    assert set(result[0]['guess']) == set(expected[0]['guess'])

def test_jsonify_works_with_int64():
    new_items = {frozenset({int64(3), int64(4)}): int64(11)}
    result_json = simulations.util.jsonify_new_items(new_items)
    result = json.loads(result_json)
    assert result[0]['result'] == 11
    assert set(result[0]['guess']) == set([3, 4])
