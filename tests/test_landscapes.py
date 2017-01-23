import pytest
import pandas
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
    assert landscape.evaluate(['Stone', 'Antler']) == 'Antler_Refined'
    assert landscape.evaluate(['Tree']) == 'Bough'

def test_evaluate_incorrect_guess(landscape):
    assert landscape.evaluate(['Stone', 'Tree']) is None
    assert landscape.evaluate(['Red_Berry']) is None

@pytest.mark.skip('proportion matching not enforced')
def test_guess_must_match_proportion(landscape):
    assert landscape.evaluate(['Stone']) is None
    assert landscape.evaluate(['Stone', 'Stone']) == 'Small_Stone'
