MAX_GUESS_SIZE = 4

class Team:
    """Teams are groups of 1 or more players."""
    def __init__(self, players):
        self.players = players
        self.active_players = []  # players must be activated to use
        self.inventory = []       # teams share an inventory

    def guesses(self):
        inventory = combine_player_inventories(*self.players)
        return [player.guess(inventory) for player in self.active_players]


class Player:
    """Players make guesses."""
    def __init__(self, rand):
        self.rand = rand

    def guess(self, inventory):
        n_items = self.rand.choice(range(1, MAX_GUESS_SIZE+1))
        return self.rand.choice(inventory, size=n_items, replace=False)
