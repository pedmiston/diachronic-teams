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

calculate_inventory_size <- function(frame, grouping_variable) {
  frame %>%
    group_by_(.dots = grouping_variable) %>%
    mutate(InventorySize = cumsum(UniqueItem)) %>%
    ungroup()
}

team_trials %<>%
  calculate_inventory_size("TeamID") %>%
  recode_strategy() %>%
  recode_groups_by_generation()

player_trials %<>%
  calculate_inventory_size("PlayerID") %>%
  recode_strategy() %>%
  recode_groups_by_generation()

team_info <- team_trials %>%
  select(TeamID, Strategy) %>%
  unique()

team_performance <- team_trials %>%
  group_by(TeamID) %>%
  summarize(
    InventorySize = max(InventorySize)
  ) %>%
  left_join(team_info) %>%
  recode_strategy()

totems_theme <- load_totems_theme()