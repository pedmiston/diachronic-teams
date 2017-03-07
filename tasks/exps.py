from os import environ
import sys
import json

from invoke import task
import pandas
import gspread
from unipath import Path
from oauth2client.service_account import ServiceAccountCredentials
from sqlalchemy import update

import graph
import db
from db import Group, Player
from tasks import paths

TOTEMS_DIR = Path(paths.R_PKG, 'data-raw/totems')
if not TOTEMS_DIR.isdir():
    TOTEMS_DIR.mkdir()

WORKSHOP_CSV = Path(TOTEMS_DIR, 'Workshop.csv')


@task
def download(ctx, name=None, analyze_after=False):
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

    if analyze_after:
        analyze(ctx)


def tables():
    con = db.connect_to_db()
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
    creds_dict = db.get_from_vault(
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
def analyze(ctx):
    """Analyze the totems experiment data."""
    workshop = pandas.read_csv(WORKSHOP_CSV)
    workshop = label_player_groups_and_strategies(workshop)
    workshop = calculate_team_time(workshop)
    workshop = rolling(workshop)
    workshop = adjacent(workshop)
    workshop = difficulty(workshop)
    workshop.to_csv(WORKSHOP_CSV, index=False)


def rolling(workshop):
    """Keep track of rolling variables (e.g., total known inventory).

    Args:
        workshop: DataFrame with columns TrialTime, WorkshopString,
                  and WorkShopResult.
    """
    landscape = graph.Landscape()

    def _rolling(workshop, prefix=''):
        """Calculate rolling variables for this player or this group."""
        guess_history = set()
        guess_history_sizes = []
        guess_uniqueness = []

        inventory = landscape.starting_inventory()
        rolling_inventory = []
        inventory_sizes = []

        for trial in workshop.sort_values('TeamTime').itertuples():
            # Record the guess
            is_unique_guess = trial.WorkShopString not in guess_history
            if is_unique_guess:
                guess_history.update({trial.WorkShopString})
            guess_uniqueness.append(int(is_unique_guess))
            guess_history_sizes.append(len(guess_history))

            # Record the result
            if trial.WorkShopResult != 0:
                label = landscape.get_label(trial.WorkShopResult)
                inventory.update({label})
            rolling_inventory.append(json.dumps(list(inventory)))
            inventory_sizes.append(len(inventory))

        workshop[prefix + 'UniqueGuess'] = guess_uniqueness
        workshop[prefix + 'NumUniqueGuesses'] = guess_history_sizes
        workshop[prefix + 'Inventory'] = rolling_inventory
        workshop[prefix + 'InventorySize'] = inventory_sizes
        return workshop

    rolling_inventories = (workshop.groupby('ID_Player').apply(_rolling)
                                   .groupby('ID_Group').apply(_rolling,
                                                              prefix='Team'))
    return rolling_inventories


def adjacent(workshop):
    """Calculate the number of adjacent possibilities for each player."""
    landscape = graph.Landscape()
    inventories = workshop.Inventory.apply(json.loads)
    workshop['NumAdjacent'] = \
        (inventories.apply(landscape.determine_adjacent_possible)
                    .apply(len))
    return workshop


def difficulty(workshop):
    """Calculate the difficulty of a particular stage in the totems game."""
    workshop['Difficulty'] = workshop.InventorySize/workshop.NumAdjacent
    return workshop


def label_player_groups_and_strategies(frame):
    con = db.connect_to_db()
    labels = pandas.read_sql("""
    SELECT ID_Player, Treatment, Table_Player.ID_Group as ID_Group, Ancestor
    FROM Table_Player
    LEFT JOIN Table_Group
    ON Table_Player.ID_Group = Table_Group.ID_Group
    """, con)

    labels['Generation'] = 1
    labels['Generation'] = (labels.Generation
                                  .where(labels.Treatment != 'Diachronic',
                                         labels.Ancestor + 2))
    labels.drop('Ancestor', axis=1, inplace=True)

    labels['ID_Player'] = labels.ID_Player.astype(int)
    return frame.merge(labels)


def calculate_team_time(workshop):
    workshop = workshop.copy()
    session_duration_sec = 25 * 60
    # Convert milliseconds to seconds
    workshop['PlayerTime'] = workshop.TrialTime * 1000
    workshop['TeamTime'] = workshop.PlayerTime.where(
        workshop.Treatment != 'Diachronic',
        workshop.PlayerTime + ((workshop.Generation - 1) * session_duration_sec),
    )
    return workshop
