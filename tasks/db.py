from invoke import task
import pandas
import db
from db import Group, Player

from .paths import *

@task
def install(ctx):
    """Install the totems database on a remote server."""
    ctx.run('cd db && ansible-playbook install.yml')


@task
def snapshot(ctx):
    """Take a snapshot of the totems database."""
    ctx.run('cd db && ansible-playbook snapshot.yml')


@task
def restore(ctx, dump):
    """Restore the totems database from a snapshot."""
    cmd = 'cd db && ansible-playbook restore.yml -e dump={dump}'
    ctx.run(cmd.format(dump=dump))


@task
def label(ctx, subj_info=None):
    """Label valid subjects (modifies Table_Group)."""
    con = db.connect_to_db()
    players = pandas.read_sql('SELECT * FROM Table_Player', con)
    groups = pandas.read_sql('SELECT * FROM Table_Group', con)
    players = players.merge(groups)

    # Verify team sizes
    actual_sizes = players.groupby('ID_Group').size()
    actual_sizes.name = 'ActualSize'
    players = players.merge(actual_sizes.reset_index())
    players['is_team_full'] = (players.Size == players.ActualSize)

    # Drop any players not in the subject info sheet
    subj_info = subj_info or Path(TOTEMS_RAW_DATA, 'SubjInfo.csv')
    if not subj_info.exists():
        raise AssertionError((
            'Subject info not found at "{}". To download it, run:'
            '\n\n\tinv experiment.download -n subj_info\n'
        ).format(subj_info))

    subj_info = pandas.read_csv(subj_info)
    players['is_known_player'] = (players.ID_Player
                                         .astype(int)
                                         .isin(subj_info.ID_Player))

    # Label those groups with known players and full teams as valid.
    group_statuses = players.groupby('ID_Group').apply(
        lambda x: all(x.is_team_full) and all(x.is_known_player)
    )
    group_statuses.name = 'is_valid_group'
    group_statuses = group_statuses.reset_index()
    group_statuses['Status'] = None
    group_statuses.Status.where(~group_statuses.is_valid_group, 'E',
                                inplace=True)
    del group_statuses['is_valid_group']
    group_statuses = group_statuses.to_dict('records')
    group_ids = [group['ID_Group']
                 for group in group_statuses
                 if group['Status'] == 'E']

    # Update the database to reflect the valid groups
    con.execute(Group.update().values(Status = None))
    con.execute(
        Group.update()
             .values(Status = 'E')
             .where(Group.c.ID_Group.in_(group_ids))
    )


@task
def close(ctx, group_id):
    """Close an open group."""
    con = db.connect_to_db()

    groups = pandas.read_sql('SELECT * FROM Table_Group', con)
    assert group_id in groups.ID_Group.tolist()

    con.execute(
        Group.update()
             .values(Open = 0)
             .where(Group.c.ID_Group == group_id)
    )
