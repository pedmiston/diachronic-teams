from unipath import Path

PROJ = Path(__file__).absolute().ancestor(2)
R_PKG = Path(PROJ, 'totems')
REPORTS = Path(PROJ, 'reports')
TOTEMS = Path(PROJ, 'experiment')
ITEM_IMAGES = Path(R_PKG, 'inst/extdata/items')
