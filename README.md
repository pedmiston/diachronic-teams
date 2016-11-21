# Diachronic Teams

This research project investigates the role of iteration and inheritance in successful collaborations by comparing the problem solving abilities of teams arranged either diachronically or synchronically.

![](/team-structures.png)

# Installing

Here's how I set up to work on this repository.

    # Clone the EvoTeams repository
    $ git clone https://github.com/pedmiston/diachronic-teams.git
    $ cd diachronic-teams

    # Setup a python virtualenv for the package
    $ mkvirtualenv --python=python3 teams -r requirements.txt

    # List all available invoke tasks
    $ inv -l
    Available tasks:

      evoteams.install      Install the evoteams R pkg.
      evoteams.use_data     Compile data to .rda in evoteams R pkg.
      kaggle.download       Download Kaggle Meta dataset.
      reports.render        Compile RMarkdown reports to their output formats.

Other requirements:

- "tidyverse" of R packages
- pandoc for dynamic documents
- graphviz for node/edge diagrams

## kaggle

To download the Kaggle data, run the following invoke task:

    $ inv kaggle.download  # opens browser to download Kaggle db
    $ mv /path/to/kaggle.sqlite evoteams/inst/extdata/kaggle.sqlite

## evoteams

An R package wrapping all of the data collected in this research. **After downloading the data** run the following invoke tasks to compile and install the R package:

    $ inv evoteams.use_data evoteams.install

## reports

Dynamic documents. **After installing the evoteams R pkg** build the reports with:

    $ inv reports.render

## experiments

Subdirectories for each of the experiments.
