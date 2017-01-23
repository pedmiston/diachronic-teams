from py2neo import Node, Relationship


def Item(number, label, generation):
    assert isinstance(number, int), 'py2neo only likes native python ints'
    return Node('Item', number=number, label=label, generation=generation)

def Recipe(code):
    return Node('Recipe', code=code)

def Creates(recipe, result):
    return Relationship(recipe, 'CREATES', result)

def Requires(recipe, requirement):
    return Relationship(recipe, 'REQUIRES', requirement)

def Inherits(result, requirement):
    return Relationship(result, 'INHERITS', requirement)
