from os import environ
from py2neo import Graph, Unauthorized


def connect_to_graph_db():
    if 'NEO4J_PASSWORD' not in environ:
        raise Unauthorized('must set NEO4J_PASSWORD env variable')
    return Graph(password=environ['NEO4J_PASSWORD'])
