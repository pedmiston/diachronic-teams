from os import environ
import py2neo


class Landscape:
    match_clause = 'MATCH (n) -[:REQUIRES]-> (:Item {{label: "{}"}})'

    def __init__(self):
        try:
            password = environ['NEO4J_PASSWORD']
        except KeyError:
            raise py2neo.Unauthorized('NEO4J_PASSWORD not set')
        self.graph = py2neo.Graph(password=password)

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
        """Given a guess, see if it made anything.

        Args:
            guess: A list of item labels in any order.
        Returns:
            An answer label (str).
        Raises:
            py2neo.Unauthorized: If access to the db is prevented.
            NoInnovationFoundError: If the guess didn't make anything.
            TooManyInnovationsFoundError: If more than one answer was found.
        """
        clauses = [self.match_clause.format(label) for label in guess]
        query = '\n'.join(['MATCH (n:Item)'] + clauses + ['RETURN n;'])
        results = self.graph.data(query)  # raises py2neo.Unauthorized

        if len(results) == 0:
            raise NoInnovationFoundError
        elif len(results) > 1:
            raise TooManyInnovationsFoundError

        answer = results[0]['n']
        return answer['label']


class NoInnovationFoundError(Exception):
    """A guess and a fail."""

class TooManyInnovationsFoundError(Exception):
    """A single guess should have a single answer."""
