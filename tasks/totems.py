from os import environ

from invoke import task
import sqlalchemy
import ansible_vault
import pandas
from unipath import Path

from . import paths

TOTEMS_DIR = Path(paths.R_PKG, 'data-raw/totems')
if not TOTEMS_DIR.isdir():
    TOTEMS_DIR.mkdir()


@task
def download(ctx):
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


def get_from_vault(key):
    ansible_vault_password_file = environ.get('ANSIBLE_VAULT_PASSWORD_FILE')
    ansible_vault_password = open(ansible_vault_password_file).read().strip()
    vault = ansible_vault.Vault(ansible_vault_password)
    secrets_yaml = Path(paths.PROJ, 'db/vars/secrets.yml')
    data = vault.load(open(secrets_yaml).read())
    return data.get(key)
