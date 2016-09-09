#!/usr/bin/env python3
import sys
import argparse
import pandas

def get_leaderboard(slug):
    url = 'https://www.kaggle.com/c/{}/leaderboard'
    tables = pandas.read_html(url.format(slug), header=0)
    leaderboard = tables[0]
    leaderboard['slug'] = slug
    return leaderboard


def get_leaderboards(slugs):
    leaderboards = [get_leaderboard(slug) for slug in slugs]
    return pandas.concat(leaderboards, ignore_index=True)
