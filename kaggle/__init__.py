import logging
import betamax
from unipath import Path

from .leaderboards import get_leaderboard, get_leaderboards
from .competitions import get_competitions, get_competition_pages

logger = logging.getLogger(__name__)
logger.addHandler(logging.StreamHandler())

CASSETTES = Path('cassettes')
if not CASSETTES.isdir():
    CASSETTES.mkdir()

with betamax.Betamax.configure() as config:
    config.cassette_library_dir = CASSETTES
    config.default_cassette_options['record_mode'] = 'new_episodes'
