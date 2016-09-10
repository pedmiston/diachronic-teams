from unipath import Path

PROJ = Path(__file__).absolute().ancestor(2)
REPORTS = Path(PROJ, 'reports')
