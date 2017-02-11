# What is the impact of inheritance on problem solving ability?

<img src="https://github.com/pedmiston/diachronic-teams/raw/master/img/team-structures-all.png" align="left" width="100">

This is a research project that investigates the impact of inheritance on problem solving ability. The simplest form of inheritance considered is one person working on a solution to a problem for a time, and then passing the solution on to a second person to continue the work. Teamwork based around inheritance is referred to as **diachronic teamwork**, which can be contrasted with **synchronic teamwork**, where teamwork is divided among team members working in parallel. The goal of this project is to understand the conditions under which diachronic teamwork is a more effective use of labor hours than synchronic teamwork. Also considered are **isolated individuals** working for the same total number of labor hours. Isolated individuals have the most time for individual learning, but may get stuck where the diversity of teams may not.

# Totems experiment

<img src="https://github.com/pedmiston/diachronic-teams/raw/master/img/landscape-sample.png" width="300">

# Reproducible

Here's how I set up to work on this project.

    $ git clone https://github.com/pedmiston/diachronic-teams.git
    $ cd diachronic-teams

    # Setup a python virtualenv for the package
    $ mkvirtualenv --python=python3 teams -r requirements/teams.txt

    # List all available invoke tasks
    $ inv -l

Make sure you have the following programs installed:

- pandoc for dynamic documents
- graphviz for node/edge diagrams

On macOS I installed these programs with [homebrew](https://brew.sh).

    $ brew install pandoc graphviz

# Data

    $ inv totems.download --post-processing  # download totems data
    $ inv simulations.run all --post-processing     # run simulations
    $ inv totems.install --use-data        # use data in totems pkg
