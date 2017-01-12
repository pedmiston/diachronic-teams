import numpy

MAX_GUESS_SIZE = 4


def create_team(n_players=1, seed=None):
    rand = numpy.random.RandomState(seed)
    players = [Player(rand) for _ in range(n_players)]
    return Team(players)


class Team:
    """Teams are groups of 1 or more players."""
    def __init__(self, players):
        self.players = players
        self.active_players = []  # players must be activated to use
        self.inventory = {'fat_tree', 'skinny_tree', 'rock_1',
                          'red_berries', 'blue_berries', 'antler'}

    def make_guesses(self):
        return [player.guess(self.inventory) for player in self.active_players]


class Player:
    """Players make guesses."""
    def __init__(self, rand=None):
        self.rand = rand or numpy.random.RandomState()

    def guess(self, inventory):
        n_items = self.rand.choice(range(1, MAX_GUESS_SIZE+1))
        return self.rand.choice(list(inventory), size=n_items, replace=False)
