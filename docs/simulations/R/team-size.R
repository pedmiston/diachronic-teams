source("docs/simulations/R/setup.R")

# ---- team-size
data("BotsPlayers")
gg_final %+% filter_final_round(BotsPlayers)
(gg_timeline %+% filter_final_round(BotsPlayers)) +
  aes(n_players, inventory_size, color = strategy) +
  scale_x_continuous("team size", breaks = unique(BotsPlayers$n_players)) +
  theme(panel.grid.minor.x = element_blank())