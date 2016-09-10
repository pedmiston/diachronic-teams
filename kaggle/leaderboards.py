#!/usr/bin/env python3
import sys
import logging
import pandas

logger = logging.getLogger(__name__)


def get_leaderboard(slug):
    url = 'https://www.kaggle.com/c/{}/leaderboard'
    tables = pandas.read_html(url.format(slug), header=0)
    leaderboard = tables[0]
    leaderboard['slug'] = slug
    return leaderboard


def get_leaderboards(slugs):
    leaderboards = []
    for slug in slugs:
        try:
            leaderboard = get_leaderboard(slug)
        except Exception:
            logger.error('error getting leaderboard {}'.format(slug))
        else:
            leaderboards.append(leaderboard)
    return pandas.concat(leaderboards, ignore_index=True)
