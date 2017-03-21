import pytest
import pandas
from py2neo import Node

import graph


@pytest.fixture
def items():
    return [Node('Item', generation=x) for x in range(5)]

def test_max_generation(items):
    assert graph.max_generation(items) == 4

def test_node_from_max_generation(items):
    Node('Item', generation=graph.max_generation(items))

def test_missing_generation_raises_exception():
    items = [Node('Item', generation=x) for x in [0, None]]
    try:
        graph.max_generation(items)
    except graph.MissingGeneration:
        pass
    else:
        raise AssertionError('expecting a MissingGeneration exception')

def test_int64_raises_exception():
    try:
        graph.models.Item(number=pandas.np.int64(1), label='test',
                               generation=0)
    except AssertionError:
        pass
    else:
        # This is a bug in py2neo, so if it works, it's fixed
        raise Exception('this test is no longer needed')


@pytest.fixture
def landscape():
    return graph.Landscape()

def test_landscape_has_max_items(landscape):
    assert landscape.max_items == 192

def test_evaluate_correct_guess(landscape):
    assert landscape.evaluate(['Stone', 'Antler']) == 'Antler_Refined'
    assert landscape.evaluate(['Tree']) == 'Bough'

def test_evaluate_correct_numeric_guess(landscape):
    assert landscape.evaluate([2]) == 13

def test_evaluate_incorrect_guess(landscape):
    assert landscape.evaluate(['Stone', 'Tree']) is None
    assert landscape.evaluate(['Red_Berry']) is None

@pytest.mark.skip('proportion matching not enforced')
def test_guess_must_match_proportion(landscape):
    assert landscape.evaluate(['Stone']) is None
    assert landscape.evaluate(['Stone', 'Stone']) == 'Small_Stone'

def test_adjacent_possible(landscape):
    assert len(landscape.adjacent_possible(['Stone'])) == 1

def test_adjacent_possible_doesnt_return_existing_items(landscape):
    recipes = landscape.adjacent_possible(['Stone', 'Small_Stone'])
    assert len(recipes) == 1

def test_starting_inventory(landscape):
    assert 1 in landscape.starting_inventory()

def test_getting_label_from_number(landscape):
    assert landscape.get_label(1) == 'Big_Tree'
