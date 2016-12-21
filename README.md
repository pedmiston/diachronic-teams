<img src="https://github.com/pedmiston/diachronic-teams/raw/master/img/team-structures.png" align="left" width="100">

# Diachronic Teams

This research project investigates the impact of inheritance on the problem solving ability of individuals. The simplest form of inheritance considered is one person working on a solution to a problem for a time, and then passing the solution on to a second person to continue the work. Because the solution is willingly given to the second person in the chain, I refer to this simple form of inheritance as a form of teamwork. Teamwork based around inheritance is referred to as **diachronic teamwork** which can be contrasted with **synchronic teamwork**, where teamwork is divided among the team members. The goal of this project is to understand the conditions under which diachronic teamwork is a more effective use of labor hours than synchronic teamwork.

# Reproducing

Here's how I set up to work on this repository.

    # Clone the repository
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
