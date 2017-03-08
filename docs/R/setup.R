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
data("team_trials")
data("player_trials")

calculate_inventory_size <- function(frame, grouping_variable) {
  frame %>%
    group_by_(.dots = grouping_variable) %>%
    mutate(InventorySize = cumsum(UniqueItem)) %>%
    ungroup()
}

calculate_unique_guesses <- function(frame, grouping_variable) {
  frame %>%
    group_by_(.dots = grouping_variable) %>%
    mutate(UniqueGuesses = cumsum(UniqueGuess)) %>%
    ungroup()
}

team_trials %<>%
  calculate_inventory_size("TeamID") %>%
  calculate_unique_guesses("TeamID") %>%
  recode_strategy() %>%
  recode_groups_by_generation()

player_trials %<>%
  calculate_inventory_size("PlayerID") %>%
  calculate_unique_guesses("PlayerID") %>%
  recode_strategy() %>%
  recode_groups_by_generation()

player_info <- player_trials %>%
  select(PlayerID, TeamID, Strategy, Generation) %>%
  unique()

player_performance <- player_trials %>%
  group_by(PlayerID) %>%
  summarize(
    InventorySize = max(InventorySize),
    UniqueGuesses = max(UniqueGuesses)
  ) %>%
  left_join(player_info) %>%
  recode_strategy()

team_info <- team_trials %>%
  select(TeamID, Strategy) %>%
  unique()

team_performance <- team_trials %>%
  group_by(TeamID) %>%
  summarize(
    InventorySize = max(InventorySize),
    UniqueGuesses = max(UniqueGuesses)
  ) %>%
  left_join(team_info) %>%
  recode_strategy()

totems_theme <- load_totems_theme()