# Install a MySQL database on a remote server

The Totems experiment requires a MySQL database to be installed on a remote server and accessible from each of the testing computers. This directory contains an [ansible](https://www.ansible.com/) playbook for installing and setting up the MySQL server.

## Prior to running the ansible playbook

- Make sure you can connect to the server via SSH. This requires `openssh-server` to be installed.
- (Optional) Allow sudo commands without password for user. This will make the playbook run without any prompts for passwords.

## Install ansible in a python virtualenv

    virtualenv --python=python2 ~/.venvs/totems
    source ~/.venvs/totems/bin/activate
    pip install -r requirements/totems-db.txt

## Add an entry into your ansible hosts file for the remote server

    # /etc/ansible/hosts
    # ...
    [totems]
    XXX.XXX.XXX.XXX

You can check that ansible can connect to your server with the following ad-hoc command.

    ansible totems -m ping

## Run the playbook

    ansible-playbook setup_totems_db.yml

## Creating a backup

Save a backup of the db on the local machine.

    ansible-playbook download_db_dump.yml
