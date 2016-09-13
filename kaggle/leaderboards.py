#!/usr/bin/env python3
import sys
import logging
import pandas
import requests
import bs4
from betamax import Betamax
from unipath import Path

logger = logging.getLogger(__name__)
session = requests.Session()

CASSETTES = Path('cassettes')
if not CASSETTES.isdir():
    CASSETTES.mkdir()

with Betamax.configure() as config:
    config.cassette_library_dir = CASSETTES
    config.default_cassette_options['record_mode'] = 'new_episodes'


def get_leaderboard(slug):
    output = 'slug team_name team_members team_size entries score'.split()
    url = 'https://www.kaggle.com/c/{}/leaderboard'

    with Betamax(session).use_cassette('leaderboards'):
        response = session.get(url.format(slug))
        html = response.content.decode()

    # Let pandas do as much as possible
    tables = pandas.read_html(html, header=0)
    leaderboard = tables[0]
    leaderboard.rename(columns=slugify, inplace=True)
    leaderboard['slug'] = slug

    # Get the team members from the soup
    soup = bs4.BeautifulSoup(html, 'html.parser')
    team_divs = [el.parent
                 for el in soup.find_all('a', class_='team-link')]
    teams = pandas.DataFrame({'divs': team_divs})
    teams['team_name'] = teams.divs.apply(find_team_name)
    teams['team_members'] = teams.divs.apply(find_team_members)
    teams['team_size'] = teams.team_members.str.split(',').apply(len)

    # Merge the teams with the leaderboard
    leaderboard['team_name'] = (leaderboard.iloc[:, 2]
                                           .str.split('  ')
                                           .str.get(0))
    leaderboard = leaderboard.merge(teams)

    return leaderboard[output]


def get_leaderboards(slugs, skip=True):
    leaderboards = []
    for slug in slugs:
        try:
            leaderboard = get_leaderboard(slug)
        except Exception as e:
            logger.error('error getting leaderboard {}'.format(slug))
            if not skip:
                raise e
        else:
            leaderboards.append(leaderboard)
    return pandas.concat(leaderboards, ignore_index=True)


def slugify(name):
    return name.lower().replace(' ', '_')


def find_team_name(div):
    return div.find('a', class_='team-link').text.strip()


def find_team_members(div):
    sep = ','
    ul = div.find('ul', class_='team-members')
    if ul:
        team_members = [a.text.strip() for a in ul.find_all('a')]
    else:
        single_player = div.find('a', class_='single-player')
        assert single_player, 'no team members but not a single player'
        team_members = [single_player.text.strip()]
    return sep.join(team_members)
