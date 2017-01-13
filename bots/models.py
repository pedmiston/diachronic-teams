import numpy

MAX_GUESS_SIZE = 4


def create_team(n_players=1, seed=None, player_memory=False, team_memory=False):
    rand = numpy.random.RandomState(seed)
    players = [Player(rand, memory=player_memory) for _ in range(n_players)]
    return Team(players, memory=team_memory)


class Team:
    """Teams are groups of 1 or more players."""
    def __init__(self, players, memory=False):
        self.players = players
        self.active_players = []  # players must be activated to use
        self.inventory = {'fat_tree', 'skinny_tree', 'rock_1',
                          'red_berries', 'blue_berries', 'antler'}
        self.history = []
        self.memory = memory

    def make_guesses(self):
        return [player.guess(self.inventory, self.history)
                for player in self.active_players]

    def update_inventory(self, new_items):
        self.inventory.update(new_items.values())
        # Only add to history the guesses that lead to new items
        if self.memory:
            for guess in new_items:
                self.history.append(set(guess))


class Player:
    """Players make guesses."""
    def __init__(self, rand=None, memory=False):
        self.rand = rand or numpy.random.RandomState()
        self.history = []
        self.memory = memory

    def guess(self, inventory, team_history):
        for successful_guess in team_history:
            if set(successful_guess) not in self.history:
                self.history.append(set(successful_guess))

        while True:
            n_items = self.rand.choice(range(1, MAX_GUESS_SIZE+1))
            guess = self.rand.choice(list(inventory), size=n_items,
                                     replace=False).tolist()
            if not self.memory:
                break
            elif set(guess) not in self.history:
                break

        self.history.append(set(guess))
        return guess
