from landscapes.graph_db import connect_to_graph_db


def determine_adjacent_possible(inventory):
    """Return a set of recipes obtainable by the given inventory."""
    inventory = set(inventory)
    graph = connect_to_graph_db()

    adjacent_query = """
    MATCH (n:Item) <-[:REQUIRES]- (r:Recipe)
    WHERE n.label IN {inventory}
    RETURN r.code as code
    """.format(inventory=list(inventory))
    adjacent_recipes = pandas.DataFrame(graph.data(adjacent_query))

    requirements_query = """
    MATCH (r:Recipe) -[:REQUIRES]-> (n:Item)
    WHERE r.code IN {codes}
    RETURN r.code as code, n.label as requirement
    """.format(codes=adjacent_recipes.code.tolist())
    requirements = pandas.DataFrame(graph.data(requirements_query))

    adjacent_possible = []
    for code, chunk in requirements.group_by('code'):
        if all(chunk.requirement.isin(inventory)):
            adjancent_possible.append(code)

    return adjacent_possible
