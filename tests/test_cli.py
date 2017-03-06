from unipath import Path
from tasks.docs import reports_from_name

def test_resports_from_name():
    expected = Path('docs/totems.Rmd').absolute()
    assert reports_from_name('totems') == [expected]
