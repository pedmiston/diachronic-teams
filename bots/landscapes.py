from os import environ
import py2neo


class Landscape:
    match_clause = 'MATCH (n) -[:REQUIRES]-> (:Item {{label: "{}"}})'

    def __init__(self):
        self.graph = py2neo.Graph(password=environ.get('NEO4J_PASSWORD'))

    def evaluate_guesses(self, guesses):
        results = []
        for guess in guesses:
            try:
                result = self.evaluate_guess(guess)
            except NoInnovationFoundError:
                pass
            else:
                results += result
        return results

    def evaluate_guess(self, guess):
        clauses = [self.match_clause.format(label) for label in guess]
        query = '\n'.join(['MATCH (n:Item)'] + clauses + ['RETURN n;'])
        result = self.graph.data(query)

        if len(result) == 0:
            raise NoInnovationFoundError

        return result[0]['n']['label']


class NoInnovationFoundError(Exception):
    """A guess and a fail."""
