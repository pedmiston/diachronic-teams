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
data("TotemsSampled")
data("TotemsPlayers")
data("TotemsTeams")

TotemsTrials %<>%
  recode_strategy() %>%
  recode_groups_by_generation()

TotemsSampled %<>%
  recode_strategy()

TotemsPlayers %<>%
  recode_strategy()

TotemsTeams %<>%
  recode_strategy()

# Get team inventories at particular time points
TotemsSampledMeans <- TotemsSampled %>%
  group_by(Strategy, SampledTime) %>%
  summarize(NumInnovations = mean(NumInnovations)) %>%
  ungroup() %>%
  recode_strategy() %>%
  guess_generation("SampledTime") %>%
  recode_groups_by_generation()

trial0 <- data_frame(
    Strategy = c("Diachronic", "Diachronic", "Synchronic", "Isolated"),
    Generation = c(1, 2, 1, 1),
    SampledTime = c(0, 60*25, 0, 0),
    NumInnovations = 0
  ) %>%
  recode_strategy() %>%
  recode_groups_by_generation()

TotemsSampledMeans %<>%
  bind_rows(trial0)

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