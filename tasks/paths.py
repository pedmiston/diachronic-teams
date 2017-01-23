from unipath import Path

PROJ = Path(__file__).absolute().ancestor(2)
R_PKG = Path(PROJ, 'evoteams')
REPORTS = Path(PROJ, 'reports')
TOTEMS = Path(PROJ, 'totems-experiment')
ITEM_IMAGES = Path(R_PKG, 'inst/extdata/items')
