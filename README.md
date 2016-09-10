# EvoTeams

This research project investigates the role of iteration in successful collaborations.

# Installing

Here's how I set up to work on this repository.

    # Clone the EvoTeams repository
    $ git clone https://github.com/pedmiston/evoteams.git EvoTeams

    # Setup a python virtualenv for the package
    $ cd EvoTeams
    $ mkvirtualenv --python=python3 teams -r requirements.txt

    # List all available invoke tasks
    $ inv -l

Other requirements:

- "tidyverse" of R packages
- pandoc for dynamic documents
- graphviz for node/edge diagrams

# Contents

    ├── kaggle            # python module for getting Kaggle data
    ├── evoteams          # R pkg wrapping all data in project
    ├── reports           # dynamic documents
    ├── tasks             # invoke tasks (CLI)
    ├── analysis.Rproj    # root level RStudio project
    └── requirements.txt  # python requirements

## kaggle

A python module for scraping data from Kaggle competition listings and leaderboards. To download the data, run the following invoke tasks:

    $ inv kaggle.competitions kaggle.leaderboards

## evoteams

An R package wrapping all of the data collected in this research. **After downloading the data** run the following invoke tasks to compile and install the R package:

    $ inv evoteams.use_data evoteams.install

## reports

Dynamic documents. **After installing the evoteams R pkg** build the reports with:

    $ inv reports.render
