import pandas

from landscapes.graph_db import connect_to_graph_db

class Landscape:
    def __init__(self):
        """Copies the landscape for faster lookups."""
        graph = connect_to_graph_db()
        recipes = pandas.DataFrame(graph.data("""
        MATCH (recipe) -[:CREATES]-> (result:Item)
        MATCH (result) -[:INHERITS]-> (requirement:Item)
        RETURN result.label as result,
               requirement.label as requirement;
        """))
        self.answer_key = {}
        for result, chunk in recipes.groupby('result'):
            requirements = frozenset(chunk.requirement.tolist())
            self.answer_key[requirements] = result

        self.max_items = graph.data("""
        MATCH (n:Item)
        RETURN count(n) as n_items
        """)[0]['n_items']  # graph.data always returns a list

    def evaluate(self, guess):
        return self.answer_key.get(frozenset(guess))

    def evaluate_guesses(self, guesses):
        new_items = {}
        for guess in guesses:
            result = self.evaluate(guess)
            if result:
                new_items[frozenset(guess)] = result
        return new_items
