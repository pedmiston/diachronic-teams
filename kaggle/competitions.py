import requests


def get_competitions(page):
    url = 'https://www.kaggle.com/competitions'
    kwargs = dict(
        sortBy='prize',
        group='all',
        page=1,
        site='main',
    )
    response = request.get(url, params=kwargs)
