from os import environ

import sqlalchemy
import ansible_vault
from unipath import Path

from .models import Group, Player


PROJ = Path(__file__).ancestor(2).absolute()


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
    secrets_yaml = Path(PROJ, vault_file)
    data = vault.load(open(secrets_yaml).read())
    if key is None:
        return data
    else:
        return data.get(key)
