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
data("TeamPerformance")

data("TeamTrials")
data("SampledTeamTrials")

data("PlayerTrials")
data("SampledPlayerTrials")


TeamTrials %<>%
  recode_strategy() %>%
  recode_groups_by_generation()

SampledTeamTrials %<>%
  recode_strategy() %>%
  recode_groups_by_generation()

SampledPlayerTrials %<>%
  recode_strategy() %>%
  recode_groups_by_generation()

PlayerTrials %<>%
  recode_strategy()

TeamPerformance %<>%
  recode_strategy()

# Get team inventories at particular time points
SampledTeamTrialsMeans <- SampledTeamTrials %>%
  group_by(Strategy, SampledTime) %>%
  summarize(NumInnovations = mean(NumInnovations)) %>%
  ungroup() %>%
  recode_strategy() %>%
  guess_generation("SampledTime") %>%
  recode_groups_by_generation()

SampledPlayerTrialsMeans <- SampledPlayerTrials %>%
  group_by(Strategy, Generation, SampledTime) %>%
  summarize(NumInnovations = mean(NumInnovations)) %>%
  ungroup() %>%
  recode_strategy() %>%
  recode_groups_by_generation()

trial0 <- data_frame(
    Strategy = c("Diachronic", "Diachronic", "Synchronic", "Isolated"),
    Generation = c(1, 2, 1, 1),
    SampledTime = c(0, 60*25, 0, 0),
    NumInnovations = 0
  ) %>%
  recode_strategy() %>%
  recode_groups_by_generation()

SampledTeamTrialsMeans %<>%
  bind_rows(trial0)

SampledPlayerTrialsMeans %<>%
  bind_rows(trial0) %>%
  filter(!(GenerationStrategy == "Diachronic-2" & SampledTime == 1500 & NumInnovations == 0))

TeamInventoryGuesses <- TeamTrials %>%
  filter(Result == 0) %>%
  group_by(TeamID, TeamInventory) %>%
  summarize(
    Guesses = n(),
    Redundancy = 1 - (sum(TeamUniqueGuess)/n())
  ) %>%
  ungroup() %>%
  left_join(select(TeamTrials, TeamID, TeamInventory, Strategy, NumTeamInnovations)) %>%
  recode_strategy()

IndividualInventoryGuesses <- TeamTrials %>%
  filter(Result == 0) %>%
  group_by(PlayerID, TeamInventory) %>%
  summarize(
    Guesses = n(),
    Redundancy = 1 - (sum(UniqueGuess)/n())
  ) %>%
  ungroup() %>%
  left_join(select(TeamTrials, PlayerID, TeamID, TeamInventory, Strategy, NumInnovations)) %>%
  recode_strategy()

totems_theme <- load_totems_theme()