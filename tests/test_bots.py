import bots

def test_evaluate_correct_guess():
    guess = ['rock_1', 'antler']
    landscape = bots.landscapes.Landscape()
    result = landscape.evaluate_guesses([guess])
    assert len(result) == 1
    assert result[0] == 'club'
