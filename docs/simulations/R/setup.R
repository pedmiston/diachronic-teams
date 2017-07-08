# ---- setup
library(tidyverse)
library(magrittr)
library(totems)
totems_theme <- load_totems_theme()

# Scale inventory size as number of innovations
totems_theme$scale_y_inventory_size <- scale_y_continuous(
  "Number of innovations",
  breaks = seq(6, 16, by = 2),
  labels = seq(0, 10, by = 2)
)

# Scales for diachronic/synchronic colors
totems_theme$scale_color_strategy_2 <- scale_color_manual(
  "Strategy",
  labels = c("Diachronic", "Synchronic"),
  values = totems_theme$color_picker(c("blue", "green"))
)
totems_theme$scale_fill_strategy_2 <- scale_fill_manual(
  "Strategy",
  labels = c("Diachronic", "Synchronic"),
  values = totems_theme$color_picker(c("blue", "green"))
)

sim_vars <- c("strategy", "n_guesses", "n_players", "seed", "player_memory", "team_memory")
filter_final_round <- . %>%
  group_by_(.dots = sim_vars) %>%
  filter(round == max(round)) %>%
  ungroup()