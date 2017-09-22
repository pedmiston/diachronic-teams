# What is the impact of inheritance on problem solving ability?

<img src="https://github.com/pedmiston/diachronic-teams/raw/master/img/team-structures.png" align="left" width="100">

This is a research project investigating the impact of inheritance on problem solving ability. Inheritance is implemented as one person working on a solution to a problem for a while, and then passing the solution on to a second person to continue. Teamwork based around inheritance is referred to as **diachronic teamwork**, which can be contrasted with **synchronic teamwork**, where teamwork is divided among team members working in parallel. The goal of this project is to understand the conditions under which diachronic teamwork is a more effective use of labor hours than synchronic teamwork. Also considered are **isolated individuals** working for the same total number of labor hours. Isolated individuals have the most time for individual learning, but may get stuck where the diversity of teams may not.

## Experiment

<img src="https://github.com/pedmiston/diachronic-teams/raw/master/img/landscape-sample.png" width="300">

## Reproducible

Here's how I set up to work on this project.

```bash
$ git clone https://github.com/pedmiston/diachronic-teams.git
$ cd diachronic-teams
$ git submodule init && git submodule update
$ python3 -m venv ~/.venvs/totems
$ source ~/.venvs/totems/bin/activate
(totems) $ pip install -r requirements.txt
(totems) $ inv -l
```
