# What is the impact of inheritance on problem solving ability?

<img src="https://github.com/pedmiston/diachronic-teams/raw/master/img/team-structures-all.png" align="left" width="100">

This is a research project that investigates the impact of inheritance on problem solving ability. The simplest form of inheritance considered is one person working on a solution to a problem for a time, and then passing the solution on to a second person to continue the work. Teamwork based around inheritance is referred to as **diachronic teamwork**, which can be contrasted with **synchronic teamwork**, where teamwork is divided among team members working in parallel. The goal of this project is to understand the conditions under which diachronic teamwork is a more effective use of labor hours than synchronic teamwork. Also considered are **isolated individuals** working for the same total number of labor hours. Isolated individuals have the most time for individual learning, but may get stuck where the diversity of teams may not.

# Totems experiment

<img src="https://github.com/pedmiston/diachronic-teams/raw/master/img/landscape-sample.png" width="300">

# Reproducible

Here's how I set up to work on this project. First clone the repo.

    $ git clone https://github.com/pedmiston/diachronic-teams.git
    $ cd diachronic-teams

Next, setup a python virtualenv for the package.

    # Option 1: Using virtualenv
    $ virtualenv --python=python3 ~/.venvs/teams
    $ source ~/.venvs/teams
    (teams) $ pip install -r requirements/teams.txt

    # Option 2: Using mkvirtualenvwrapper
    $ mkvirtualenv --python=python3 teams -r requirements/teams.txt

Make sure you have the following programs installed:

- pandoc for dynamic documents
- neo4j for installing the landscape as a graph database
- graphviz for node/edge diagrams

On macOS I installed these programs with [homebrew](https://brew.sh).

    $ brew install pandoc neo4j graphviz

The last step is to set some environment variables. You need to set `NEO4J_PASSWORD` and `ANSIBLE_VAULT_PASSWORD_FILE`. I store these in an untracked file named "environment" in the project root.

    #!/usr/bin/env bash
    # environment for diachronic-teams project
    export NEO4J_PASSWORD=my-neo4j-password
    export ANSIBLE_VAULT_PASSWORD_FILE=path/to/password-file.txt

Now I can work on this project by activating the python virtualenv and sourcing the correct environment variables. Be sure to start the neo4j server manually if necessary.

    $ source environment
    $ neo4j start

Project-related operations can be run from the command line as invoke tasks. To list all available invoke tasks, type the following:

    $ invoke --list  # list available tasks
    $ inv -l         # same as above!
    $ inv -h [task]  # get help on a particular task

# Data

    $ inv experiment.download all --analyze-after    # download totems data
    $ inv simulations.run all --analyze-after        # run simulations
    $ inv totems.install --use-data                  # install data in totems pkg
