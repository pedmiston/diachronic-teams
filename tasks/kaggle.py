from invoke import task
import pandas

import kaggle


@task
def download(ctx, competitions=None):
    """Download current leaderboards from Kaggle competitions."""
    competitions_csv = competitions or 'kaggle/competitions.csv'
    competitions = pandas.read_csv(competitions_csv)
    assert 'slug' in competitions
    output_csv = 'evoteams/data-raw/kaggle/leaderboards.csv'
    leaderboards = kaggle.get_leaderboards(competitions.slug)
    leaderboards.to_csv(output_csv, index=False)
