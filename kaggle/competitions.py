import requests
import pandas


def get_competitions(page):
    url = 'https://www.kaggle.com/competitions.json'
    kwargs = dict(
        sortBy='numberOfTeams',
        group='all',
        page=page,
        site='main',
    )
    response = requests.get(url, params=kwargs)
    records = response.json()['competitions']
    competitions = pandas.DataFrame.from_records(records)
    # Extract slug from last component of URL
    competitions['slug'] = (competitions.competitionUrl
                                        .str.split('/')
                                        .str.get(-1))
    return competitions

def get_competition_pages(n_pages):
    pages = [get_competitions(n) for n in range(1, n_pages)]
    competitions = pandas.concat(pages, ignore_index=True)
    competitions.drop_duplicates(inplace=True)
    return competitions
