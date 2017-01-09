
class Diachronic:
    pass


class Synchronic:
    pass


def get(label):
    return dict(diachronic=Diachronic, synchronic=Synchronic)[label]()
