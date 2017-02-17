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
def process(ctx, name=None, suffix=None):
    """Process the experiment data from the totems database."""
    available = ['rolling', 'adjacent']
    if name is None:
        names = available
    else:
        assert name in available
        names = [name]

    workshop = pandas.read_csv(WORKSHOP_CSV)

    if 'rolling' in names:
        workshop = rolling(workshop)

    if 'adjacent' in names:
        workshop = adjacent(workshop)

    if 'difficulty' in names:
        workshop = difficulty(workshop)

    if suffix:
        new_name = '{}-{}.csv'.format(WORKSHOP_CSV.stem, suffix)
        WORKSHOP_CSV = Path(WORKSHOP_CSV.parent, new_name)

    workshop.to_csv(WORKSHOP_CSV, index=False)


def rolling(workshop):
    """Keep track of rolling variables (e.g., total known inventory)."""
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
    return rolling_inventories


def adjacent(workshop):
    """Calculate the number of adjacent possibilities for each player."""
    landscape = landscapes.Landscape()
    inventories = workshop.Inventory.apply(json.loads)
    workshop['NumAdjacent'] = \
        (inventories.apply(landscape.determine_adjacent_possible)
                    .apply(len))
    return workshop


def difficulty(workshop):
    pass
