import pytest
from py2neo import Node

import landscapes


@pytest.fixture
def items():
    return [Node('Item', generation=x) for x in range(5)]

def test_max_generation(items):
    assert landscapes.max_generation(items) == 4

def test_node_from_max_generation(items):
    Node('Item', generation=landscapes.max_generation(items))

def test_missing_generation_raises_exception():
    items = [Node('Item', generation=x) for x in [0, None]]
    try:
        landscapes.max_generation(items)
    except landscapes.MissingGeneration:
        pass
    else:
        raise AssertionError('expecting a MissingGeneration exception')

def test_int64_raises_exception():
    try:
        landscapes.models.Item(number=pandas.np.int64(1), label='test',
                               generation=0)
    except AssertionError:
        pass
    else:
        # This is a bug in py2neo, so if it works, it's fixed
        raise Exception('this test is no longer needed')


@pytest.fixture
def landscape():
    return landscapes.Landscape()

def test_landscape_has_max_items(landscape):
    assert landscape.max_items == 192

def test_evaluate_correct_guess(landscape):
    guess = ['rock_1', 'antler']
    result = landscape.evaluate(guess)
    assert result == 'club'

def test_evaluate_incorrect_guess_raises_error(landscape):
    guess = ['rock_1', 'skinny_tree']
    try:
        landscape.evaluate(guess)
    except bots.landscapes.NoInnovationFoundError:
        pass
    else:
        assert False, 'should have raised an error'

def test_evaluate_incorrect_guesses_returns_empty_list(landscape):
    guesses = [['rock_1', 'skinny_tree']]
    new_items = landscape.evaluate(guesses)
    assert new_items == {}

def test_evaluate_partially_correct_guess_returns_correct_answer(landscape):
    guess = ['rock_1']
    result = landscape.evaluate(guess)
    assert result == 'rock_2'

def test_evaluate_partially_correct_guess_fails_if_not_complete(landscape):
    guess = ['red_berries']
    try:
        landscape.evaluate(guess)
    except bots.landscapes.NoInnovationFoundError:
        pass
    else:
        assert False, 'should have raised an error'
