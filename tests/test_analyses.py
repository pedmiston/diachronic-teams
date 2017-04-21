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


def test_synchronic_group_rolling_inventory():
    workshop = pandas.DataFrame({
        'ID_Player': [1] * 3 + [2],
        'ID_Group': [1] * 4,
        'TeamTime': range(4),
        'WorkShopString': ['a', 'b', 'c', 'a'],
        'WorkShopResult': [7, 8, 9, 7]
    })
    result = tasks.exps.rolling_history(workshop, prefix='Team')
    assert result.TeamUniqueGuess.tolist() == [1, 1, 1, 0]

    # Make sure it works ok with the groupby mechanic
    result = (workshop.groupby('ID_Group')
                      .apply(tasks.exps.rolling_history, prefix='Team')
                      .reset_index(drop=True))
    assert result.TeamUniqueGuess.tolist() == [1, 1, 1, 0]


def test_synchronic_fixture_rolling_inventory():
    workshop = pandas.read_csv('tests/fixtures/workshop-synchronic.csv')
    result = tasks.exps.rolling_history(workshop, prefix='Team')
    inventory_lengths = result.sort_values('TeamTime').TeamInventory.apply(len)
    assert all(inventory_lengths.diff()[1:] >= 0)

    # Make sure it works ok with the groupby mechanic
    result = (workshop.groupby('ID_Group')
                      .apply(tasks.exps.rolling_history, prefix='Team')
                      .reset_index(drop=True))
    inventory_lengths = result.sort_values('TeamTime').TeamInventory.apply(len)
    assert all(inventory_lengths.diff()[1:] >= 0)


def test_calculate_team_time(workshop):
    result = tasks.exps.calculate_team_time(workshop)
    duration_minutes = 25
    duration_seconds = duration_minutes * 60
    assert result.TeamTime.tolist() == (result.PlayerTime + duration_seconds).tolist()
