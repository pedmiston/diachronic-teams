from unipath import Path

from tasks.reports import _parse_names
from tasks.paths import REPORTS

class TestReportNameOptions:
    def test_none_returns_all(self):
        assert len(_parse_names('reports', None)) == len(list(REPORTS.walk('*.Rmd')))

    def test_pattern_match(self):
        pattern = 'modeling'
        assert len(_parse_names('reports', pattern)) == len(list(REPORTS.walk(pattern)))
