from os import environ
import py2neo


class Landscape:
    def __init__(self):
        self.graph = py2neo.Graph(password=environ.get('NEO4J_PASSWORD'))

    def evaluate_guesses(self, guesses):
        pass
