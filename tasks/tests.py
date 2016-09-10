from unipath import Path

from .reports import _parse_names
from .paths import REPORTS

class TestReportNameOptions:
    def test_none_returns_all(self):
        assert len(_parse_names(None)) == len(list(REPORTS.walk('*.Rmd')))

    def test_pattern_match(self):
        pattern = 'proposal'
        assert len(_parse_names(pattern)) == len(list(REPORTS.walk(pattern)))
