# Save the data from the totems experiment
library(devtools)
library(tidyverse)
library(magrittr)
library(lubridate)
source("R/util.R")  # Don't load "totems" or use_data() might not work properly

# Start here
read_csvs("data-raw/totems", prefix = "totems_")

# Deidentification -------------------------------------------------------------
# Remove datetime information from survey data
totems_subjinfo %<>% select(-Date, -Room)
totems_postexperimentsurvey %<>% select(-Timestamp)

# Remove datetime information from group identifier
team_id_levels <- factor(totems_group$ID_Group) %>% levels()
team_id_labels <- paste0("G", seq_along(team_id_levels))
team_id_map <- data_frame(
  ID_Group = team_id_levels,
  TeamID = team_id_labels
)

deidentify_group_id <- . %>% left_join(team_id_map) %>% select(-ID_Group)
totems_group        %<>% deidentify_group_id()
totems_player       %<>% deidentify_group_id()
totems_playertrials %<>% deidentify_group_id()
totems_teamtrials   %<>% deidentify_group_id()

# Recode player id -------------------------------------------------------------
# Create a new PlayerID variable that is a character instead of a number
player_id_levels <- unique(totems_player$ID_Player)
player_id_labels <- paste0("P", seq_along(player_id_levels))
player_id_map <- data_frame(
  ID_Player = player_id_levels,
  PlayerID = player_id_labels
)

recode_player_id <- . %>% left_join(player_id_map) %>% select(-ID_Player)
totems_subjinfo     %<>% recode_player_id()
totems_player       %<>% recode_player_id()
totems_playertrials %<>% recode_player_id()
totems_teamtrials   %<>% recode_player_id()

# Select valid participants ----------------------------------------------------
# Valid participants are those recorded in the subject info sheet
valid_participant_ids <- totems_subjinfo$PlayerID
filter_valid_participants <- . %>% filter(PlayerID %in% valid_participant_ids)
totems_player       %<>% filter_valid_participants()
totems_playertrials %<>% filter_valid_participants()
totems_teamtrials   %<>% filter_valid_participants()

# Rename treatment to strategy -------------------------------------------------
totems_group %<>% rename(Strategy = Treatment)

# Select valid teams -----------------------------------------------------------
# Remove incomplete diachronic teams
incomplete_diachronic_teams <- totems_player %>%
  left_join(totems_group %>% rename(ExpectedSize = Size)) %>%
  filter(Strategy == "Diachronic") %>%
  group_by(TeamID, ExpectedSize) %>%
  summarize(TeamSize = n()) %>%
  ungroup() %>%
  filter(TeamSize != ExpectedSize) %>%
  .$TeamID

filter_valid_teams <- . %>% filter(!(TeamID %in% incomplete_diachronic_teams))
totems_group        %<>% filter_valid_teams()
totems_player       %<>% filter_valid_teams()
totems_playertrials %<>% filter_valid_teams()
totems_teamtrials   %<>% filter_valid_teams()

# Identify players within teams and generations --------------------------------
player_generations <- totems_player %>%
  left_join(totems_group) %>%
  mutate(Generation = ifelse(Strategy == "Diachronic", Ancestor + 2, 1)) %>%
  select(PlayerID, Generation)

player_ixs <- totems_player %>%
  group_by(TeamID) %>%
  transmute(PlayerID, PlayerIX = 1:n()) %>%
  ungroup()

identify_players_in_teams <- . %>%
  left_join(player_generations) %>%
  left_join(player_ixs)

totems_player       %<>% identify_players_in_teams()
totems_playertrials %<>% identify_players_in_teams()
totems_teamtrials   %<>% identify_players_in_teams()

# Convert trial times in seconds to R native time objects ----------------------
convert_times <- . %>%
  mutate(
    PlayerTime = dseconds(PlayerTime),
    TeamTime   = dseconds(TeamTime)
  )
totems_playertrials %<>% convert_times()
totems_teamtrials   %<>% convert_times()

# Record guesses ---------------------------------------------------------------
count_guesses <- . %>%
  arrange(TeamTime) %>%
  mutate(GuessNum = 1:n())
totems_playertrials %<>% group_by(PlayerID) %>% count_guesses() %>% ungroup()
totems_teamtrials   %<>% group_by(TeamID)   %>% count_guesses() %>% ungroup()

# Save data to package ---------------------------------------------------------
select_trials_data <- . %>%
  left_join(totems_group) %>%
  select(PlayerID, TeamID, Strategy, Generation,
         TeamTime, PlayerTime, GuessNum,
         Guess = WorkShopString, Result = WorkShopResult,
         UniqueGuess, UniqueItem,
         Inventory, NumAdjacent)

player_trials <- totems_playertrials %>% select_trials_data()
team_trials   <- totems_teamtrials   %>% select_trials_data()

use_data(
  player_trials,
  team_trials,
  overwrite = TRUE
)
