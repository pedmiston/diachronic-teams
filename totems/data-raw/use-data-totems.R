library(devtools)
library(tidyverse)
library(lubridate)

# Source functions from the package without loading the whole thing.
# WARNING! Don't load the whole "totems" package
# or use_data() might not work properly.
library(magrittr)
library(tools)
library(readr)
source("R/util.R")

# Reads in all csvs in data-raw/totems and saves them with a "totems_" prefix
read_csvs("data-raw/totems", prefix = "totems_")

# Player scores ----------------------------------------------------------------
totems_players <- totems_player %>%
  left_join(totems_group) %>%
  mutate(Generation = ifelse(Treatment == "Diachronic", Ancestor + 2, 1)) %>%
  rename(Strategy = Treatment) %>%
  select(-c(ID_Number:Gain, Knowledge, Size, Open, Status)) %>%
  # Filter out any subjects that aren't reported
  # in the subject info sheet.
  inner_join(totems_subjinfo)

# Remove any incomplete diachronic teams
incomplete_diachronic_teams <- totems_players %>%
  filter(Strategy == "Diachronic") %>%
  group_by(ID_Group) %>%
  summarize(TeamSize = max(Generation)) %>%
  filter(TeamSize != 2) %>%
  .$ID_Group
totems_players %<>% filter(!(ID_Group %in% incomplete_diachronic_teams))

# ----------------- Start Deidentification -----------------
# Remove datetime information and shuffle rows.
set.seed(328)
id_group_levels <- factor(totems_players$ID_Group) %>% levels()
id_group_labels <- paste0("G", seq_along(id_group_levels))
totems_players %<>%
  select(-Date, -Room) %>%
  mutate(
    ID_Group = factor(ID_Group, levels = id_group_levels, labels = id_group_labels),
    ID_Group = as.character(ID_Group)
  ) %>%
  sample_n(nrow(totems_players))
# ----------------- End Deidentification --------------------

player_key <- totems_players %>%
  select(ID_Player, ID_Group, Strategy, Generation)

team_key <- player_key %>%
  select(ID_Group, Strategy) %>%
  unique()

# Workshops --------------------------------------------------------------------
totems_workshops <- totems_workshopanalyzed %>%
  # Remove ID_Group column because it's been renamed for anonymity
  # in the player_key
  select(-ID_Group) %>%
  inner_join(player_key)

# Enumerate each player's guesses
totems_workshops %<>%
  group_by(ID_Player) %>%
  mutate(GuessNumber = 1:n()) %>%
  ungroup()

# Enumerate each team's guesses
totems_workshops %<>%
  group_by(ID_Group) %>%
  arrange(TrialTime) %>%
  mutate(TeamGuessNumber = 1:n()) %>%
  ungroup()

# Calculate accumulated difficulty score
workshop_difficulties <- totems_workshops %>%
  arrange(ID_Player, GuessNumber) %>%
  select(ID_Player, InventorySize, Difficulty) %>%
  unique() %>%
  group_by(ID_Player) %>%
  mutate(DifficultyScore = cumsum(Difficulty)) %>%
  ungroup()
totems_workshops %<>% left_join(workshop_difficulties)

# Add vars derived from workshop data to leaderboards
player_workshop_summaries <- totems_workshops %>%
  group_by(ID_Player) %>%
  summarize(
    InventorySize = max(InventorySize),
    DifficultyScore = max(DifficultyScore),
    Attempts = max(GuessNumber),
    TeamAttempts = max(TeamGuessNumber),
    UniqueGuesses = max(NumUniqueGuesses),
    TeamUniqueGuesses = max(TeamNumUniqueGuesses)
  )
totems_players %<>% left_join(player_workshop_summaries)

# Convert seconds to R native time objects
totems_workshops %<>%
  mutate(
    PlayerTime = dseconds(PlayerTime),
    TeamTime = dseconds(TeamTime)
  )

# Teams ------------------------------------------------------------------------
totems_teams <- totems_players %>%
  group_by(ID_Group) %>%
  summarize(
    Score = max(Score),
    InventorySize = max(InventorySize),
    DifficultyScore = max(DifficultyScore),
    Attempts = max(Attempts),
    TeamAttempts = max(TeamAttempts),
    TeamUniqueGuesses = max(TeamUniqueGuesses)
  ) %>%
  left_join(team_key)

# Use data in package ----------------------------------------------------------
totems_teams %<>%
  select(ID_Group, Strategy,
         Score, InventorySize, DifficultyScore,
         TeamAttempts,
         TeamUniqueGuesses)

totems_players %<>%
  select(ID_Player, Strategy, Generation, ID_Group,
         Score, InventorySize, DifficultyScore,
         Attempts, TeamAttempts,
         UniqueGuesses, TeamUniqueGuesses)

totems_workshops %<>%
  select(ID_Player, Strategy, Generation, ID_Group,
         GuessString = WorkShopString, GuessResult = WorkShopResult,
         NumAdjacent, Difficulty, DifficultyScore,
         PlayerTime, GuessNumber, UniqueGuess, NumUniqueGuesses,
         Inventory, InventorySize,
         TeamTime, TeamGuessNumber, TeamUniqueGuess, TeamNumUniqueGuesses,
         TeamInventory, TeamInventorySize)

use_data(
  totems_teams,
  totems_players,
  totems_workshops,
  overwrite = TRUE
)
