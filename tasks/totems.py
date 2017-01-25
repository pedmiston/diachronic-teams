from os import environ

from invoke import task
import sqlalchemy
import ansible_vault
import pandas
import gspread
from unipath import Path
from oauth2client.service_account import ServiceAccountCredentials

from . import paths

TOTEMS_DIR = Path(paths.R_PKG, 'data-raw/totems')
if not TOTEMS_DIR.isdir():
    TOTEMS_DIR.mkdir()


@task
def download(ctx, all=False):
    """Download the data from the totems db."""
    url = "mysql+pymysql://{user}:{password}@{host}:{port}/{dbname}".format(
        user='experimenter',
        password=get_from_vault('experimenter_password'),
        host='128.104.130.116',
        port='3306',
        dbname='Totems',
    )
    con = sqlalchemy.create_engine(url)
    for table in con.table_names():
        frame = pandas.read_sql('SELECT * FROM %s' % table, con)
        out_csv = Path(TOTEMS_DIR, '{}.csv'.format(table.split('_')[1]))
        frame.to_csv(out_csv, index=False)

    if all:
        subj_info(ctx)
        survey(ctx)


@task
def subj_info(ctx):
    """Download the subject info sheet from Google Drive."""
    df = get_worksheet('totems-subj-info')
    df.rename(columns=dict(SubjID='ID_Player',
                           Initials='Experimenter'),
              inplace=True)
    cols = 'ID_Player Strategy Date Room Experimenter Compliance'.split()
    df[cols].to_csv(Path(TOTEMS_DIR, 'SubjInfo.csv'), index=False)


@task
def survey(ctx):
    """Download the survey responses from Google Drive."""
    df = get_worksheet('totems-survey-responses')
    df.to_csv(Path(TOTEMS_DIR, 'PostExperimentSurvey.csv'), index=False)


def get_from_vault(key=None, vault_file='db/vars/secrets.yml'):
    ansible_vault_password_file = environ.get('ANSIBLE_VAULT_PASSWORD_FILE')
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
