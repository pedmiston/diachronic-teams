# ---- setup
base_theme <- theme_minimal()

strategy_colors <- RColorBrewer::brewer.pal(3, "Set2")[c(1, 3)]
scale_color_strategy <- scale_color_manual(values = strategy_colors)
scale_fill_strategy <- scale_fill_manual(values = strategy_colors)

# inventory
min_inventory_size <- 6
max_inventory_size <- 33
scale_y_inventory_size <- scale_y_continuous("size of inventory",
                                             breaks = c(min_inventory_size, max_inventory_size,
                                                        seq(10, 30, by = 5)))
coord_inventory_lim <- c(min_inventory_size, max_inventory_size)

gg_timeline <- ggplot() +
  aes(round, inventory_size, color = strategy) +
  geom_line(stat = "summary", fun.y = "mean") +
  scale_color_strategy +
  scale_y_inventory_size +
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
  theme(legend.position = "none")

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