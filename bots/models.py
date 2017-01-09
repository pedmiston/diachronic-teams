MAX_GUESS_SIZE = 4


def create_team(n_players):
    pass


class Team:
    """Teams are groups of 1 or more players."""
    def __init__(self, players):
        self.players = players
        self.active_players = []  # players must be activated to use

    def guesses(self):
        inventory = combine_player_inventories(*self.players)
        return [player.guess(inventory) for player in self.active_players]


class Player:
    """Players make guesses."""
    def __init__(self, rand):
        self.rand = rand

    def guess(self, inventory):
        while True:
            n_items = self.rand.poisson(2)
            if n_items > 0 and n_items < MAX_GUESS_SIZE:
                break
        return self.rand.choice(inventory, size=n_items, replace=False)


def combine_player_inventories(*players):
    inventory = dict()
    for player in players:
        inventory.update(player.inventory)
    return inventory.values()
