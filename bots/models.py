import numpy

MAX_GUESS_SIZE = 4


def create_team(n_players=1, seed=None, memory=False):
    rand = numpy.random.RandomState(seed)
    players = [Player(rand, memory=memory) for _ in range(n_players)]
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
    def __init__(self, rand=None, memory=False):
        self.rand = rand or numpy.random.RandomState()
        self.history = []
        self.memory = memory

    def guess(self, inventory):
        while True:
            n_items = self.rand.choice(range(1, MAX_GUESS_SIZE+1))
            guess = self.rand.choice(list(inventory), size=n_items,
                                     replace=False)
            if not self.memory:
                break
            elif set(guess) not in self.history:
                break

        self.history.append(set(guess))
        return guess
