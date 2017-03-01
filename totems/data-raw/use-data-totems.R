library(devtools)
library(tidyverse)

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
  mutate(Generation = Ancestor + 2) %>%
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
totems_workshops <- totems_workshop %>%
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

# Calculate team time
# For diachronic teams, TeamTime should reflect running total.
duration_minutes <- 25
duration_msec <- duration_minutes * 60 * 1000
totems_workshops %<>%
  left_join(player_key) %>%
  mutate(TeamTime = TrialTime + (Generation - 1) * duration_msec)

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
    TeamAttempts = max(TeamGuessNumber)
  )
totems_players %<>% left_join(player_workshop_summaries)

# Teams ------------------------------------------------------------------------
totems_teams <- totems_players %>%
  group_by(ID_Group) %>%
  summarize(
    Score = max(Score),
    InventorySize = max(InventorySize),
    DifficultyScore = max(DifficultyScore),
    Attempts = max(Attempts),
    TeamAttempts = max(TeamAttempts)
  ) %>%
  left_join(team_key)

# Use data in package ----------------------------------------------------------
totems_teams %<>%
  select(ID_Group, Strategy, Score, InventorySize, DifficultyScore, TeamAttempts)

totems_players %<>%
  select(ID_Player, Strategy, Generation, ID_Group, Score, InventorySize, DifficultyScore, Attempts, TeamAttempts)

totems_workshops %<>%
  select(ID_Player, Strategy, Generation, ID_Group,
         TeamTime, GuessNumber, TeamGuessNumber,
         GuessString = WorkShopString, GuessResult = WorkShopResult,
         Inventory, InventorySize, NumAdjacent,
         Difficulty, DifficultyScore)

use_data(
  totems_teams,
  totems_players,
  totems_workshops,
  overwrite = TRUE
)
