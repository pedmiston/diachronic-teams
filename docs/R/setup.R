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
data("totems_teams")
data("totems_players")
data("totems_workshops")

totems_teams %<>%
  recode_strategy()

totems_players %<>%
  recode_strategy()

totems_workshops %<>%
  recode_strategy()

team_key <- select(totems_teams, ID_Group, Strategy)
player_key <- select(totems_players, ID_Player, ID_Group, Strategy, Generation)

team_ratchets <- totems_teams %>%
  select(
    ID_Group, TeamAttempts,
    Score, InventorySize, DifficultyScore
  ) %>%
  gather(Measure, Value, -(ID_Group:TeamAttempts)) %>%
  recode_score_value() %>%
  left_join(team_key) %>%
  recode_strategy()

player_ratchets <- totems_players %>%
  select(
    ID_Player, Attempts, TeamAttempts,
    Score, InventorySize, DifficultyScore
  ) %>%
  gather(Measure, Value, -(ID_Player:TeamAttempts)) %>%
  gather(AttemptMeasure, Attempts, -c(ID_Player, Measure, Value)) %>%
  recode_score_value() %>%
  recode_attempt_measures() %>%
  left_join(player_key) %>%
  recode_strategy()

totems_theme <- load_totems_theme()