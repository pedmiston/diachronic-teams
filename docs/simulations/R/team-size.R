source("docs/simulations/R/setup.R")

# ---- team-size
data("BotsPlayers")

BotsPlayersFinal <- filter_final_round(BotsPlayers)

ggplot(BotsPlayersFinal) +
  aes(n_players, inventory_size, color = strategy) +
  geom_line(stat = "summary", fun.y = "mean", size = 1) +
  geom_point(position = position_jitter(1), alpha = 0.2, shape = 1) +
  scale_x_continuous("Team size") +
  totems_theme$scale_y_inventory_size +
  totems_theme$scale_color_strategy_2 +
  totems_theme$base_theme
