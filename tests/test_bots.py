import bots

def test_evaluate_correct_guess():
    guess = ['rock_1', 'antler']
    landscape = bots.landscapes.Landscape()
    result = landscape.evaluate_guess(guess)
    assert result == 'club'

def test_evaluate_incorrect_guess():
    guess = ['rock_1', 'skinny_tree']
    landscape = bots.landscapes.Landscape()
    try:
        landscape.evaluate_guess(guess)
    except bots.landscapes.NoInnovationFoundError:
        pass
    else:
        assert False, 'Should have raised an error'
