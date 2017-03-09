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

landscape = graph.Landscape()


@task
def download(ctx, name=None, clear_before=False, analyze_after=False):
    """Download the experiment data from the totems database."""
    available = ['tables', 'subj_info', 'survey']

    if name is None:
        names = available
    else:
        assert name in available
        names = [name]

    if clear_before:
        TOTEMS_DIR.rmtree()
        TOTEMS_DIR.mkdir()

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
    df.to_csv(Path(TOTEMS_DIR, 'Survey.csv'), index=False)


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
    workshop = pandas.read_csv(Path(TOTEMS_DIR, 'Workshop.csv'))
    workshop = label_teams_and_strategies(workshop)
    workshop = calculate_team_time(workshop)

    workshop = workshop.groupby('ID_Player').apply(rolling_history)
    workshop = workshop.groupby('ID_Group').apply(rolling_history, prefix='Team')

    workshop = determine_adjacent_possible(workshop)

    write_trials_to_csv(workshop, 'WorkshopAnalyzed.csv')


def rolling_history(trials, prefix=''):
    """Keep track of rolling variables, like total known inventory."""
    trials = trials.copy()
    rolling_inventory = landscape.starting_inventory()
    rolling_guesses = set()

    inventories = []
    unique_item = []
    unique_guess = []

    for trial in trials.sort_values('TeamTime').itertuples():
        # Record the guess
        is_unique_guess = trial.WorkShopString not in rolling_guesses
        if is_unique_guess:
            rolling_guesses.update({trial.WorkShopString})
        unique_guess.append(int(is_unique_guess))

        # Record the result of the guess
        is_guess_successful = trial.WorkShopResult != 0
        is_unique_item = 0  # default
        if is_guess_successful:
            label = landscape.get_label(trial.WorkShopResult)
            is_unique_item = label not in rolling_inventory
            if is_unique_item:
                rolling_inventory.update({label})
        unique_item.append(int(is_unique_item))

        # Store the current rolling inventory
        inventories.append(list(rolling_inventory))

    trials[prefix+'Inventory'] = inventories
    trials[prefix+'UniqueItem'] = unique_item
    trials[prefix+'UniqueGuess'] = unique_guess
    return trials


def determine_adjacent_possible(trials):
    """Calculate the number of adjacent possibilities for each player."""
    trials = trials.copy()
    inventories = trials.Inventory
    trials['NumAdjacent'] = (trials.Inventory
                                   .apply(landscape.adjacent_possible)
                                   .apply(len))
    return trials


def label_teams_and_strategies(frame):
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


def calculate_team_time(trials):
    trials = trials.copy()
    session_duration_sec = 25 * 60
    # Convert milliseconds to seconds
    trials['PlayerTime'] = trials.TrialTime/1000
    trials['TeamTime'] = trials.PlayerTime.where(
        trials.Treatment != 'Diachronic',
        trials.PlayerTime + ((trials.Generation - 1) * session_duration_sec),
    )
    return trials


def write_trials_to_csv(trials, csv_name):
    trials = freeze_inventories(trials)
    trials.to_csv(Path(TOTEMS_DIR, csv_name), index=False)


def freeze_inventories(trials):
    trials = trials.copy()
    trials['Inventory'] = trials.Inventory.apply(freeze_inventory)
    return trials


def freeze_inventory(inventory):
    return json.dumps(sorted(inventory))
