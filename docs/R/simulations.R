# ---- simulations
library(tidyverse)
library(magrittr)
library(grid)
library(gridExtra)
library(jsonlite)
library(totems)

totems_theme <- load_totems_theme()

base_theme <- totems_theme["base_theme"]

strategy_colors <- RColorBrewer::brewer.pal(3, "Set2")[c(1, 3)]
scale_color_strategy <- scale_color_manual("Strategy", values = strategy_colors)
scale_fill_strategy <- scale_fill_manual("Strategy", values = strategy_colors)

# inventory
scale_y_inventory_size <- scale_y_continuous("Number of innovations",
                                             breaks = seq(6, 16, by = 2),
                                             labels = seq(0, 10, by = 2))
coord_inventory_lim <- c(6, 16.5)

gg_timeline <- ggplot() +
  aes(round, inventory_size, color = strategy) +
  geom_line(stat = "summary", fun.y = "mean", size = 1.2) +
  scale_x_continuous("Round") +
  scale_y_inventory_size +
  scale_color_strategy +
  coord_cartesian(ylim = coord_inventory_lim) +
  base_theme +
  theme(legend.position = "top")

gg_final <- ggplot() +
  aes(strategy, inventory_size) +
  geom_bar(aes(fill = strategy), stat = "summary", fun.y = "mean",
           alpha = 0.6) +
  geom_point(aes(color = strategy), position = position_jitter(0.3),
             shape = 1) +
  scale_y_inventory_size +
  coord_cartesian(ylim = coord_inventory_lim) +
  scale_color_strategy +
  scale_fill_strategy +
  base_theme +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )

sim_vars <- c("strategy", "n_guesses", "n_players", "seed", "player_memory", "team_memory")

filter_final_round <- . %>%
  group_by_(.dots = sim_vars) %>%
  filter(round == max(round)) %>%
  ungroup()

recode_team_memory <- function(frame) {
  team_memory_levels <- c("no team memory", "yes team memory")
  team_memory_map <- data_frame(
    team_memory = c(0, 1),
    team_memory_label = factor(team_memory_levels, levels = rev(team_memory_levels))
  )
  left_join(frame, team_memory_map)
}

recode_player_memory <- function(frame) {
  player_memory_levels <- c("no player memory", "yes player memory")
  player_memory_map <- data_frame(
    player_memory = c(0, 1),
    player_memory_label = factor(player_memory_levels, levels = player_memory_levels)
  )
  left_join(frame, player_memory_map)
}

recode_memory <- . %>%
  recode_team_memory() %>%
  recode_player_memory()