from unipath import Path

PROJ = Path(__file__).absolute().ancestor(2)
R_PKG = Path(PROJ, 'evoteams')
REPORTS = Path(PROJ, 'reports')
EXPERIMENTS = Path(PROJ, 'experiments')
