# ---- setup
library(tidyverse)
library(totems)
totems_theme <- load_totems_theme()

# Scale inventory size as number of innovations
totems_theme$scale_y_inventory_size <- scale_y_continuous(
  "Number of innovations",
  breaks = seq(6, 16, by = 2),
  labels = seq(0, 10, by = 2)
)


sim_vars <- c("strategy", "n_guesses", "n_players", "seed", "player_memory", "team_memory")
filter_final_round <- . %>%
  group_by_(.dots = sim_vars) %>%
  filter(round == max(round)) %>%
  ungroup()