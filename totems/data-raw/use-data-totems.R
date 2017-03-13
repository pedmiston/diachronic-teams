# Save the data from the totems experiment
library(devtools)
library(tidyverse)
library(lazyeval)
library(magrittr)
library(lubridate)
source("R/util.R")  # Don't load "totems" or use_data() might not work properly

# Start here
read_csvs("data-raw/totems")

# Replace workshop with analyzed version
Workshop <- WorkshopAnalyzed

# Deidentification -------------------------------------------------------------
# Remove datetime information from subject info sheet and survey data
SubjInfo %<>% select(-Date, -Room)
Survey %<>% select(-Timestamp)

# Remove datetime information from group identifier
team_id_levels <- factor(Group$ID_Group) %>% levels()
team_id_labels <- paste0("G", seq_along(team_id_levels))
team_id_map <- data_frame(
  ID_Group = team_id_levels,
  TeamID = team_id_labels
)

deidentify_group_id <- . %>% left_join(team_id_map) %>% select(-ID_Group)
Group    %<>% deidentify_group_id()
Player   %<>% deidentify_group_id()
Workshop %<>% deidentify_group_id()
Trajectories %<>% deidentify_group_id()

# Recode player id -------------------------------------------------------------
# Create a new PlayerID variable that is a character instead of a number.
player_id_levels <- unique(Player$ID_Player)
player_id_labels <- paste0("P", seq_along(player_id_levels))
player_id_map <- data_frame(
  ID_Player = player_id_levels,
  PlayerID = player_id_labels
)

recode_player_id <- . %>% left_join(player_id_map) %>% select(-ID_Player)
SubjInfo %<>% recode_player_id()
Player   %<>% recode_player_id()
Workshop %<>% recode_player_id()
Trajectories %<>% recode_player_id()
Survey %<>% rename(ID_Player = `Participant ID`) %>% recode_player_id()

# Select valid participants ----------------------------------------------------
# Valid participants are those recorded in the subject info sheet
valid_participant_ids <- SubjInfo$PlayerID
filter_valid_participants <- . %>% filter(PlayerID %in% valid_participant_ids)

Player   %<>% filter_valid_participants()
Workshop %<>% filter_valid_participants()
Survey   %<>% filter_valid_participants()
Trajectories %<>% filter_valid_participants()

# Rename treatment to strategy -------------------------------------------------
Group %<>% rename(Strategy = Treatment)

# Select valid teams -----------------------------------------------------------
# Create a list of incomplete diachronic teams
incomplete_diachronic_teams <- Player %>%
  left_join(rename(Group, ExpectedSize = Size)) %>%
  filter(Strategy == "Diachronic") %>%
  group_by(TeamID, ExpectedSize) %>%
  summarize(TeamSize = n()) %>%
  ungroup() %>%
  filter(TeamSize != ExpectedSize) %>%
  .$TeamID

# Label diachronic teams with invalid generation
invalid_diachronic_teams <- read_csv("data-raw/verified-diachronic-teams.csv") %>%
  deidentify_group_id() %>%
  # Count as "invalid" teams that are not valid
  # and teams whose validity is unknown.
  filter(ValidTeam == "n" | ValidTeam == "u") %>%
  .$TeamID %>%
  unique()

filter_valid_teams <- . %>%
  filter(!(TeamID %in% incomplete_diachronic_teams),
         !(TeamID %in% invalid_diachronic_teams))

Group    %>% filter_valid_teams()
Player   %<>% filter_valid_teams()
Workshop %<>% filter_valid_teams()
Trajectories %<>% filter_valid_teams()

# Identify players within teams and generations --------------------------------
player_generations <- Player %>%
  left_join(Group) %>%
  # Ancestor is coded as -1 for first generation, and +1 for everyone else
  mutate(Generation = ifelse(Strategy == "Diachronic", Ancestor + 2, 1)) %>%
  select(PlayerID, Generation)

identify_players_in_teams <- . %>% left_join(player_generations)

Player   %<>% identify_players_in_teams()
Workshop %<>% identify_players_in_teams()
Trajectories %<>% identify_players_in_teams()

# Convert trial times in seconds to R native time objects ----------------------
convert_times <- . %>%
  mutate(
    PlayerTime = dseconds(PlayerTime),
    TeamTime   = dseconds(TeamTime)
  )
Workshop %<>% convert_times()

# Count guesses ----------------------------------------------------------------
count_guesses <- function(frame, grouping_var, prefix="") {
  guess_col_name <- paste0(prefix, "GuessNum")
  mutate_call <- interp(~ 1:n())
  frame %>%
    group_by_(.dots = grouping_var) %>%
    arrange(TeamTime) %>%
    mutate_(.dots = setNames(list(mutate_call), guess_col_name)) %>%
    ungroup()
}
Workshop %<>%
  count_guesses("PlayerID") %>%
  count_guesses("TeamID", "Team")

# Calculate cumulative performance variables -----------------------------------
Workshop %<>%
  group_by(PlayerID) %>%
  mutate(
    NumInnovations = cumsum(UniqueItem),
    NumUniqueGuesses = cumsum(UniqueGuess)
  ) %>%
  group_by(TeamID) %>%
  mutate(
    NumTeamInnovations = cumsum(TeamUniqueItem),
    NumTeamUniqueGuesses = cumsum(TeamUniqueGuess)
  ) %>%
  ungroup()

# Save data to package ---------------------------------------------------------
TotemsTrials <- Workshop %>%
  left_join(Group) %>%
  select(PlayerID, TeamID, Strategy, Generation,
         TeamTime, PlayerTime, GuessNum, TeamGuessNum,
         Guess = WorkShopString, Result = WorkShopResult,
         UniqueGuess, TeamUniqueGuess, UniqueItem, TeamUniqueItem,
         Inventory, TeamInventory, NumAdjacent,
         NumInnovations, NumTeamInnovations,
         NumUniqueGuesses, NumTeamUniqueGuesses)

TotemsPlayers <- Workshop %>%
  left_join(Group) %>%
  group_by(PlayerID, TeamID, Strategy, Generation) %>%
  summarize(
    NumGuesses = max(GuessNum),
    NumInnovations = max(NumInnovations),
    NumUniqueGuesses = max(NumUniqueGuesses)
  ) %>%
  ungroup()

TotemsTeams <- Workshop %>%
  left_join(Group) %>%
  group_by(TeamID, Strategy) %>%
  summarize(
    NumInnovations = max(NumTeamInnovations),
    NumGuesses = max(TeamGuessNum),
    NumUniqueGuesses = max(NumTeamUniqueGuesses)
  )

use_data(
  TotemsTrials,
  TotemsPlayers,
  TotemsTeams,
  Trajectories,
  overwrite = TRUE
)
