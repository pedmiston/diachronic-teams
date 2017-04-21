# Save the csvs from the Totems experiment as rda data files in the R pkg

library(devtools)
library(tidyverse)
library(lazyeval)
library(magrittr)
library(lubridate)
# Don't load "totems" or use_data() might not work properly
source("R/util.R")
source("R/time-bins.R")

# Start here!
read_csvs("data-raw/totems")
source("data-raw/deidentify-totems-data.R")    # Deidentify totems data
source("data-raw/filter-valid-totems-data.R")  # Filter valid players and teams

# TeamInfo ---------------------------------------------------------------------
TeamInfo <- Group %>%
  rename(Strategy = Treatment) %>%
  select(TeamID, Strategy)

# PlayerInfo -------------------------------------------------------------------
PlayerInfo <- Player %>%
  left_join(Group) %>%
  rename(Strategy = Treatment) %>%
  mutate(Generation = ifelse(Strategy != "Diachronic", 1, Ancestor)) %>%
  select(PlayerID, TeamID, Strategy, Generation) %>%
  arrange(Strategy, TeamID, Generation)

# Guesses ----------------------------------------------------------------------
Guesses <- WorkshopAnalyzed %>%
  rename(Guess = WorkShopString, Result = WorkShopResult) %>%
  left_join(PlayerInfo) %>%
  count_guesses("PlayerID", "GuessNum") %>%
  count_guesses("TeamID", "TeamGuessNum") %>%
  select(
    PlayerID,
    PlayerTime, TeamTime,
    GuessNum, TeamGuessNum,
    Guess, Result,
    UniqueGuess, TeamUniqueGuess,
    UniqueItem, TeamUniqueItem
  )

# Inventories ------------------------------------------------------------------
calculate_num_innovations <- . %>%
  left_join(PlayerInfo) %>%
  group_by(TeamID) %>%
  arrange(TeamTime) %>%
  mutate(NumInnovations = cumsum(TeamUniqueItem)) %>%
  ungroup()

summarize_performance_on_inventory <- . %>%
  group_by(TeamID, TeamInventory, NumInnovations) %>%
  summarize(
    TeamGuesses = n(),
    TeamUniqueGuesses = sum(TeamUniqueGuess),
    UniqueGuesses = sum(UniqueGuess),
    Duration = max(TeamTime) - min(TeamTime)
  ) %>%
  ungroup() %>%
  arrange(TeamID, NumInnovations)

Inventories <- WorkshopAnalyzed %>%
  calculate_num_innovations() %>%
  summarize_performance_on_inventory()

# Performance ------------------------------------------------------------------
TeamPerformance <- Guesses %>%
  left_join(PlayerInfo) %>%
  group_by(TeamID) %>%
  summarize(
    NumInnovations = sum(TeamUniqueItem),
    NumGuesses = max(TeamGuessNum),
    NumUniqueGuesses = sum(TeamUniqueGuess)
  ) %>%
  left_join(TeamInfo)

PlayerPerformance <- Guesses %>%
  group_by(PlayerID) %>%
  summarize(
    NumInnovations = sum(TeamUniqueItem),
    NumGuesses = max(GuessNum),
    NumUniqueGuesses = sum(TeamUniqueGuess),
    NumTeamUniqueGuesses = sum(TeamUniqueGuess)
  )

# SampledPerformance -----------------------------------------------------------
calculate_num_team_innovations <- . %>%
  group_by(TeamID) %>%
  arrange(TeamTime) %>%
  mutate(NumTeamInnovations = cumsum(TeamUniqueItem)) %>%
  ungroup()

calculate_num_player_innovations <- . %>%
  group_by(PlayerID) %>%
  arrange(TeamTime) %>%
  mutate(NumInnovations = cumsum(UniqueItem)) %>%
  ungroup()

SampledPerformance <- Guesses %>%
  left_join(PlayerInfo) %>%
  calculate_num_team_innovations() %>%
  calculate_num_player_innovations() %>%
  group_by(PlayerID) %>%
  # Sample closest trial every 60 seconds
  do({ get_closest_trials_to_times(., times = seq(0, 50 * 60, by = 60)) }) %>%
  ungroup() %>%
  # Prevent Synchronic teams from being sampled outside their range.
  filter(!(Strategy == "Synchronic" & SampledTime > 25*60)) %>%
  group_by(PlayerID, SampledTime) %>%
  summarize(
    NumInnovations = max(NumInnovations),
    NumTeamInnovations = max(NumTeamInnovations)
  ) %>%
  ungroup() %>%
  left_join(PlayerInfo) %>%
  mutate(
    SampledPlayerTime = ifelse(Strategy != "Diachronic", SampledTime,
                               SampledTime - (Generation - 1) * (25 * 60))
  ) %>%
  select(
    PlayerID, SampledTime, SampledPlayerTime, NumInnovations, NumTeamInnovations
  )

# Save data to package ---------------------------------------------------------
use_data(
  TeamInfo,
  PlayerInfo,
  Guesses,
  Inventories,
  TeamPerformance,
  PlayerPerformance,
  SampledPerformance,
  overwrite = TRUE
)
