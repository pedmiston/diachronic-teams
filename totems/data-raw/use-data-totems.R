# Save the data from the totems experiment

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
  select(
    PlayerID, TeamID, Strategy, Generation
  ) %>%
  arrange(Strategy, TeamID, Generation)

# Guesses ----------------------------------------------------------------------
Guesses <- WorkshopAnalyzed %>%
  rename(Guess = WorkShopString, Result = WorkShopResult) %>%
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

# Performance ------------------------------------------------------------------
TeamPerformance <- Guesses %>%
  group_by(TeamID, Strategy) %>%
  summarize(
    NumInnovations = sum(TeamUniqueItem),
    NumGuesses = max(TeamGuessNum),
    NumUniqueGuesses = sum(TeamUniqueGuess)
  )

PlayerPerformance <- Guesses %>%
  group_by(PlayerID) %>%
  summarize(
    NumGuesses = max(GuessNum),
    NumUniqueGuesses = sum(UniqueGuess)
  )

calculate_player_performance <- . %>%
  group_by(PlayerID) %>%
  mutate(
    NumInnovations = cumsum(UniqueItem),
    NumUniqueGuesses = cumsum(UniqueGuess)
  ) %>%
  ungroup()

calculate_team_performance <- . %>%
  group_by(TeamID) %>%
  mutate(
    NumTeamInnovations = cumsum(TeamUniqueItem),
    NumTeamUniqueGuesses = cumsum(TeamUniqueGuess)
  )

Inventories <- Workshop %>%
  group_by(PlayerID, TeamInventory)



# Sample team inventories at particular time points ----------------------------
SampledTeamTrials <- Workshop %>%
  left_join(Group) %>%
  group_by(TeamID) %>%
  do({
    get_closest_trials_to_times(., times = seq(0, 50 * 60, by = 60))
  }) %>%
  ungroup() %>%
  # Prevent Synchronic teams from being sampled outside their range.
  filter(!(Strategy == "Synchronic" & SampledTime > 25*60)) %>%
  select(
    PlayerID, TeamID, Strategy, Generation,
    SampledTime, GuessNum, TeamGuessNum,
    Guess = WorkShopString, Result = WorkShopResult,
    UniqueGuess, TeamUniqueGuess, UniqueItem, TeamUniqueItem,
    Inventory, TeamInventory, NumAdjacent,
    NumInnovations, NumTeamInnovations,
    NumUniqueGuesses, NumTeamUniqueGuesses
  )

SampledPlayerTrials <- Workshop %>%
  left_join(Group) %>%
  group_by(PlayerID) %>%
  do({
    get_closest_trials_to_times(., times = seq(0, 50 * 60, by = 60),
                                time_col = "PlayerTime")
  }) %>%
  ungroup() %>%
  # Prevent Synchronic and Diachronic teams from being sampled outside their range.
  filter(!(Strategy %in% c("Synchronic", "Diachronic") & SampledTime > 25*60)) %>%
  select(
    PlayerID, TeamID, Strategy, Generation,
    SampledTime, GuessNum, TeamGuessNum,
    Guess = WorkShopString, Result = WorkShopResult,
    UniqueGuess, TeamUniqueGuess, UniqueItem, TeamUniqueItem,
    Inventory, TeamInventory, NumAdjacent,
    NumInnovations, NumTeamInnovations,
    NumUniqueGuesses, NumTeamUniqueGuesses
  )

# Summarize performance for each inventory -------------------------------------
TeamProblems <- Workshop %>%
  filter(WorkShopResult == 0) %>%
  group_by(TeamID, TeamInventory) %>%
  summarize(
    Guesses = n() + 1,
    Redundancy = 1 - (sum(TeamUniqueGuess)/n())
  ) %>%
  ungroup() %>%
  left_join(
    TeamTrials %>%
      select(TeamID, TeamInventory, Strategy, NumTeamInnovations) %>%
      unique()
  )

PlayerProblems <- Workshop %>%
  filter(WorkShopResult == 0) %>%
  group_by(PlayerID, Inventory) %>%
  summarize(
    Guesses = n() + 1,
    Redundancy = 1 - (sum(UniqueGuess)/n())
  ) %>%
  ungroup() %>%
  left_join(
    PlayerTrials %>%
      select(PlayerID, TeamID, Inventory, Strategy, NumInnovations) %>%
      unique()
  )

# Summarize team and player performance ----------------------------------------
TeamPerformance <- Workshop %>%
  left_join(Group) %>%
  group_by(TeamID, Strategy) %>%
  summarize(
    NumInnovations = max(NumTeamInnovations),
    NumGuesses = max(TeamGuessNum),
    NumUniqueGuesses = max(NumTeamUniqueGuesses)
  )

PlayerPerformance <- Workshop %>%
  left_join(Group) %>%
  group_by(PlayerID, TeamID, Strategy, Generation) %>%
  summarize(
    NumInnovations = max(NumInnovations),
    NumGuesses = max(GuessNum),
    NumUniqueGuesses = max(NumUniqueGuesses)
  )

# Save data to package ---------------------------------------------------------
use_data(
  TeamPerformance,
  TeamProblems,
  TeamTrials,
  SampledTeamTrials,
  PlayerProblems,
  SampledPlayerTrials,
  Trajectories,
  overwrite = TRUE
)
