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
def leaderboards(ctx, competitions=None):
    """Download current leaderboards from Kaggle competitions."""
    competitions_csv = competitions or Path(DST, 'competitions.csv')
    competitions = pandas.read_csv(competitions_csv)
    assert 'slug' in competitions
    leaderboards = kaggle.get_leaderboards(competitions.slug)
    leaderboards.to_csv(Path(DST, 'leaderboards.csv'), index=False)
