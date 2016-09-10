import logging

from .leaderboards import get_leaderboard, get_leaderboards
from .competitions import get_competitions, get_competition_pages

logger = logging.getLogger(__name__)
logger.addHandler(logging.StreamHandler())
