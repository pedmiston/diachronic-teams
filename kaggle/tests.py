from .leaderboards import get_leaderboard

def test_leaderboard_has_team_members():
    leaderboard = get_leaderboard('datasciencebowl')
    test_case = leaderboard.team_name == 'Happy Lantern Festival'
    assert test_case.sum() == 1, 'test case not found'
    team_members = leaderboard.ix[test_case, 'team_members'].tolist()[0]
    assert len(team_members.split(',')) == 5, 'incorrect number of team members'
