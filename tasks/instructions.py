from invoke import task
import os
import json
import ansible_vault
import gspread
import pandas
import pypandoc
import subprocess
from oauth2client.service_account import ServiceAccountCredentials

@task
def download(ctx, start_id=None, end_id=None, open_after=False):
    """Download the instructions written by participants."""
    credentials = decrypt_secrets_file("secrets/lupyanlab.json")
    gc = gspread.authorize(credentials)
    wks = gc.open("totems-survey-instructions-responses")
    sheet = wks.sheet1
    values = sheet.get_all_values()
    data = pandas.DataFrame(values[1:], columns=["timestamp", "instructions", "ancestor_id"])
    data["ancestor_id"] = data.ancestor_id.astype(int)
    start_id = int(start_id) if start_id else min(data.ancestor_id)
    end_id = int(end_id) if end_id else max(data.ancestor_id)
    sub = data.loc[(data.ancestor_id >= start_id) & (data.ancestor_id <= end_id), ]
    text = ""
    for row in sub.itertuples():
        text += f"# {row.ancestor_id}\n\n{row.instructions}\n\n\\newpage\n"
    outputfile = f"instructions-{start_id}-{end_id}.pdf"
    pypandoc.convert_text(text, "pdf", format="md", outputfile=outputfile,
                          extra_args=["--template", "tasks/instructions.template"])
    if open_after:
        subprocess.call(["open", outputfile])


def decrypt_secrets_file(encrypted_file):
    password_file = os.environ["ANSIBLE_VAULT_PASSWORD_FILE"]
    vault = ansible_vault.Vault(password=open(password_file).read().strip())
    contents = vault.load(open(encrypted_file).read())
    credentials = ServiceAccountCredentials.from_json_keyfile_dict(contents, scopes=['https://spreadsheets.google.com/feeds', 'https://www.googleapis.com/auth/drive'])
    return credentials
