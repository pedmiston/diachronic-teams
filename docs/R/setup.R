# ---- setup
library(tidyverse)
library(lubridate)
library(magrittr)
library(grid)
library(gridExtra)
library(lme4)
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

# Get team inventories at particular time points
TotemsSampled <- TotemsTrials %>%
  group_by(TeamID) %>%
  do({ get_closest_trials_to_times(., times = seq(0, 50 * 60, by = 60)) }) %>%
  filter(!(Strategy == "Synchronic" & SampledTime > 25*60))

TotemsSampledMeans <- TotemsSampled %>%
  group_by(Strategy, SampledTime) %>%
  summarize(NumInnovations = mean(NumInnovations)) %>%
  ungroup() %>%
  recode_strategy() %>%
  guess_generation("SampledTime") %>%
  recode_groups_by_generation()

# Summarize guesses at each stage (each inventory)
TeamInventoryGuesses <- TotemsTrials %>%
  filter(Result == 0) %>%
  group_by(TeamID, TeamInventory) %>%
  summarize(
    Guesses = n(),
    Redundancy = 1 - (sum(TeamUniqueGuess)/n())
  ) %>%
  ungroup() %>%
  left_join(select(TotemsTrials, TeamID, TeamInventory, Strategy, NumTeamInnovations)) %>%
  recode_strategy()

IndividualInventoryGuesses <- TotemsTrials %>%
  filter(Result == 0) %>%
  group_by(PlayerID, TeamInventory) %>%
  summarize(
    Guesses = n(),
    Redundancy = 1 - (sum(UniqueGuess)/n())
  ) %>%
  ungroup() %>%
  left_join(select(TotemsTrials, PlayerID, TeamID, TeamInventory, Strategy, NumInnovations)) %>%
  recode_strategy()

totems_theme <- load_totems_theme()