from os import environ
import sys
import json

from invoke import task
import pandas
import gspread
from unipath import Path
from oauth2client.service_account import ServiceAccountCredentials
from sqlalchemy import update

import landscapes
import database
from database import Group, Player
from tasks import paths

TOTEMS_DIR = Path(paths.R_PKG, 'data-raw/totems')
if not TOTEMS_DIR.isdir():
    TOTEMS_DIR.mkdir()

WORKSHOP_CSV = Path(TOTEMS_DIR, 'Workshop.csv')


@task
def download(ctx, name=None, post_processing=False):
    """Download the experiment data from the totems database."""
    available = ['tables', 'subj_info', 'survey']

    if name is None:
        names = available
    else:
        assert name in available
        names = [name]

    if 'tables' in names:
        tables()
    if 'subj_info' in names:
        subj_info()
    if 'survey' in names:
        survey()

    if post_processing:
        process(ctx)


def tables():
    con = database.connect_to_db()
    for table in con.table_names():
        frame = pandas.read_sql('SELECT * FROM %s' % table, con)
        out_csv = Path(TOTEMS_DIR, '{}.csv'.format(table.split('_')[1]))
        frame.to_csv(out_csv, index=False)


def subj_info(sanitize=True, save_as=True):
    """Download the subject info sheet from Google Drive."""
    df = get_worksheet('totems-subj-info')
    df.rename(columns=dict(SubjID='ID_Player',
                           Initials='Experimenter'),
              inplace=True)
    cols = 'ID_Player Strategy Date Room Experimenter Compliance'.split()
    # Sanitize!
    if sanitize:
        for col in cols:
            try:
                df[col] = df[col].str.replace('\n', '')
            except AttributeError:
                pass

    if save_as:
        df[cols].to_csv(Path(TOTEMS_DIR, 'SubjInfo.csv'), index=False)

    return df[cols]


def survey():
    """Download the survey responses from Google Drive."""
    df = get_worksheet('totems-survey-responses')
    df.to_csv(Path(TOTEMS_DIR, 'PostExperimentSurvey.csv'), index=False)


def get_worksheet(title):
    creds_dict = database.get_from_vault(
        vault_file='secrets/lupyanlab-service-account.json'
    )
    credentials = ServiceAccountCredentials.from_json_keyfile_dict(
        creds_dict, scopes='https://spreadsheets.google.com/feeds')

    gc = gspread.authorize(credentials)

    try:
        ws = gc.open(title).sheet1
    except gspread.SpreadsheetNotFound:
        print('spreadsheet %s not found, is it shared with the creds email?' % title)

    return pandas.DataFrame(ws.get_all_records())


@task
def process(ctx, name=None):
    """Process the experiment data from the totems database."""
    available = ['rolling', 'adjacent']
    if name is None:
        names = available
    else:
        assert name in available
        names = [name]

    if 'rolling' in names:
        rolling()

    if 'adjacent' in names:
        adjacent()


def rolling(suffix=None):
    """Keep track of rolling variables (e.g., total known inventory)."""
    global WORKSHOP_CSV
    workshop = pandas.read_csv(WORKSHOP_CSV)
    landscape = landscapes.Landscape()

    def _rolling(workshop):
        inventory = landscape.starting_inventory()
        rolling_inventory = []
        inventory_sizes = []
        for item_number in workshop.sort_values('TrialTime').WorkShopResult:
            if item_number != 0:
                label = landscape.get_label(item_number)
                if label not in inventory:
                    inventory.update({label})
            rolling_inventory.append(json.dumps(list(inventory)))
            inventory_sizes.append(len(inventory))
        workshop['Inventory'] = rolling_inventory
        workshop['InventorySize'] = inventory_sizes
        return workshop

    rolling_inventories = workshop.groupby('ID_Player').apply(_rolling)

    if suffix:
        new_name = '{}-{}.csv'.format(workshop_csv.stem, suffix)
        WORKSHOP_CSV = Path(WORKSHOP_CSV.parent, new_name)

    rolling_inventories.to_csv(WORKSHOP_CSV, index=False)


def adjacent(suffix=None):
    """Calculate the number of adjacent possibilities for each player."""
    global WORKSHOP_CSV
    workshop = pandas.read_csv(WORKSHOP_CSV)
    landscape = landscapes.Landscape()
    inventories = workshop.Inventory.apply(json.loads)
    workshop['NumAdjacent'] = \
        (inventories.apply(landscape.determine_adjacent_possible)
                    .apply(len))
    if suffix:
        new_name = '{}-{}.csv'.format(inventories, suffix)
        WORKSHOP_CSV = Path(WORKSHOP_CSV.parent, new_name)
    workshop.to_csv(WORKSHOP_CSV, index=False)


@task
def label(ctx):
    """Label valid subjects."""
    con = database.connect_to_db()
    players = pandas.read_sql('SELECT * FROM Table_Player', con)
    groups = pandas.read_sql('SELECT * FROM Table_Group', con)
    players = players.merge(groups)

    # Verify team sizes
    actual_sizes = players.groupby('ID_Group').size()
    actual_sizes.name = 'ActualSize'
    players = players.merge(actual_sizes.reset_index())
    players['is_team_full'] = (players.Size == players.ActualSize)

    # Drop any players not in the subject info sheet
    subj_info_sheet = subj_info(save_as=False)
    players['is_known_player'] = (players.ID_Player
                                         .astype(int)
                                         .isin(subj_info_sheet.ID_Player))

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
