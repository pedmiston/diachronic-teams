from invoke import task
from unipath import Path
import pandas

import kaggle


ROOT = Path(__file__).absolute().ancestor(2)
R_PKG = Path(ROOT, 'evoteams')
DST = Path(R_PKG, 'data-raw/kaggle')

if not DST.isdir():
    DST.mkdir(True)


@task
def competitions(ctx, n_pages=10):
    """Scrape pages of Kaggle competition listings."""
    competitions = kaggle.get_competition_pages(n_pages)
    competitions.to_csv(Path(DST, 'competitions.csv'), index=False)


@task
def leaderboards(ctx, competitions=None, stop=False):
    """Download current leaderboards from Kaggle competitions."""
    if competitions and Path(competitions).exists():
        # A path to a custom competitions file was given
        slugs = read_competition_slugs_from_file(competitions)
    elif competitions:
        # Get leaderboard for a single competition
        slugs = [competitions]
    else:
        # Look for competitions in expected location
        slugs = read_competition_slugs_from_file(Path(DST, 'competitions.csv'))

    leaderboards = kaggle.get_leaderboards(slugs, skip=not stop)
    leaderboards.to_csv(Path(DST, 'leaderboards.csv'), index=False)


def read_competition_slugs_from_file(csv):
    competitions = pandas.read_csv(csv)
    assert 'slug' in competitions
    return competitions.slug
