# ---- setup
library(tidyverse)
library(lubridate)
library(magrittr)
library(grid)
library(gridExtra)
library(broom)
library(crotchet)
library(totems)

# ---- data
library(totems)
data("TotemsTrials")
data("TotemsPlayers")
data("TotemsTeams")

TotemsTrials %<>%
  recode_strategy() %>%
  recode_groups_by_generation()

TotemsPlayers %<>%
  recode_strategy()

TotemsTeams %<>%
  recode_strategy()

totems_theme <- load_totems_theme()