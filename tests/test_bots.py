import pytest
import bots


@pytest.fixture
def landscape():
    return bots.landscapes.Landscape()

def test_evaluate_correct_guess(landscape):
    guess = ['rock_1', 'antler']
    result = landscape.evaluate_guess(guess)
    assert result == 'club'

def test_evaluate_incorrect_guess_raises_error(landscape):
    guess = ['rock_1', 'skinny_tree']
    try:
        landscape.evaluate_guess(guess)
    except bots.landscapes.NoInnovationFoundError:
        pass
    else:
        assert False, 'should have raised an error'

def test_evaluate_incorrect_guesses_returns_empty_list(landscape):
    guesses = [['rock_1', 'skinny_tree']]
    new_items = landscape.evaluate_guesses(guesses)
    assert new_items == []

def test_evaluate_partially_correct_guess_returns_correct_answer(landscape):
    guess = ['rock_1']
    result = landscape.evaluate_guess(guess)
    assert result == 'rock_2'

def test_evaluate_partially_correct_guess_fails_if_not_complete(landscape):
    guess = ['red_berries']
    try:
        landscape.evaluate_guess(guess)
    except bots.landscapes.NoInnovationFoundError:
        pass
    else:
        assert False, 'should have raised an error'

def test_team_initial_inventory():
    team = bots.models.create_team()
    assert len(team.inventory) == 6
