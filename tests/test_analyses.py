import pytest
import pandas
import tasks


@pytest.fixture
def workshop():
    return pandas.DataFrame({
        'ID_Player': [1] * 3,
        'ID_Group': [1] * 3,
        'TrialTime': range(3),
        'TeamTime': range(3),
        'Treatment': ['Diachronic'] * 3,
        'Generation': [2] * 3,
        'WorkShopString': ['a', 'a', 'b'],
        'WorkShopResult': [23, 0, 11],
    })


def test_rolling_inventory(workshop):
    result = tasks.exps.rolling_history(workshop)
    assert result.UniqueGuess.tolist() == [1, 0, 1]


def test_calculate_team_time(workshop):
    result = tasks.exps.calculate_team_time(workshop)
    duration_minutes = 25
    duration_seconds = duration_minutes * 60
    assert result.TeamTime.tolist() == (result.PlayerTime + duration_seconds).tolist()
