from os import environ
import sys
import json

from invoke import task
import sqlalchemy
import ansible_vault
import pandas
import gspread
from unipath import Path
from oauth2client.service_account import ServiceAccountCredentials

import landscapes
from tasks import paths

TOTEMS_DIR = Path(paths.R_PKG, 'data-raw/totems')
if not TOTEMS_DIR.isdir():
    TOTEMS_DIR.mkdir()

WORKSHOP_CSV = Path(TOTEMS_DIR, 'Workshop.csv')


@task
def download(ctx, post_processing=False):
    """Download the data from the totems db."""
    con = connect_to_db()
    for table in con.table_names():
        frame = pandas.read_sql('SELECT * FROM %s' % table, con)
        out_csv = Path(TOTEMS_DIR, '{}.csv'.format(table.split('_')[1]))
        frame.to_csv(out_csv, index=False)

    if post_processing:
        subj_info(ctx)
        survey(ctx)
        rolling(ctx)
        adjacent(ctx)


@task
def snapshot(ctx):
    """Take a snapshot of the totems database."""
    ctx.run('cd db && ansible-playbook download_db_dump.yml')


@task
def label(ctx):
    """Label valid subjects."""
    con = connect_to_db()
    players = pandas.read_sql('SELECT * FROM Table_Player', con)
    groups = pandas.read_sql('SELECT * FROM Table_Group', con)
    players = players.merge(groups)

    # Verify team sizes
    actual_sizes = players.groupby('ID_Group').size()
    actual_sizes.name = 'ActualSize'
    players = players.merge(actual_sizes.reset_index())
    players['is_team_full'] = players.Size == players.ActualSize

    # Drop any players not in the subject info sheet
    players['is_known_player'] = (players.ID_Player
                                         .astype(int)
                                         .isin(subj_info(ctx).ID_Player))

    groups = players.ix[players.is_team_full & players.is_known_player, ['ID_Player', 'ID_Group']]
    groups.to_csv(sys.stdout, index=False)
    

@task
def rolling(ctx, suffix=None):
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


@task
def adjacent(ctx, suffix=None):
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
def subj_info(ctx):
    """Download the subject info sheet from Google Drive."""
    df = get_worksheet('totems-subj-info')
    df.rename(columns=dict(SubjID='ID_Player',
                           Initials='Experimenter'),
              inplace=True)
    cols = 'ID_Player Strategy Date Room Experimenter Compliance'.split()
    # Sanitize!
    for col in cols:
        try:
            df[col] = df[col].str.replace('\n', '')
        except AttributeError:
            pass
    df[cols].to_csv(Path(TOTEMS_DIR, 'SubjInfo.csv'), index=False)
    return df[cols]


@task
def survey(ctx):
    """Download the survey responses from Google Drive."""
    df = get_worksheet('totems-survey-responses')
    df.to_csv(Path(TOTEMS_DIR, 'PostExperimentSurvey.csv'), index=False)


def connect_to_db():
    url = "mysql+pymysql://{user}:{password}@{host}:{port}/{dbname}".format(
        user='experimenter',
        password=get_from_vault('experimenter_password'),
        host='128.104.130.116',
        port='3306',
        dbname='Totems',
    )
    con = sqlalchemy.create_engine(url)
    return con

def get_from_vault(key=None, vault_file='db/vars/secrets.yml'):
    try:
        ansible_vault_password_file = environ['ANSIBLE_VAULT_PASSWORD_FILE']
    except KeyError:
        raise AssertionError('Set the ANSIBLE_VAULT_PASSWORD_FILE environment variable')
    ansible_vault_password = open(ansible_vault_password_file).read().strip()
    vault = ansible_vault.Vault(ansible_vault_password)
    secrets_yaml = Path(paths.PROJ, vault_file)
    data = vault.load(open(secrets_yaml).read())
    if key is None:
        return data
    else:
        return data.get(key)


def get_worksheet(title):
    credentials = ServiceAccountCredentials.from_json_keyfile_dict(
        get_from_vault(vault_file='secrets/lupyanlab-service-account.json'),
        scopes='https://spreadsheets.google.com/feeds')

    gc = gspread.authorize(credentials)

    try:
        ws = gc.open(title).sheet1
    except gspread.SpreadsheetNotFound:
        print('spreadsheet %s not found, is it shared with the creds email?' % title)

    return pandas.DataFrame(ws.get_all_records())
