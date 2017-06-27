source("docs/simulations/R/setup.R")

# ---- memory
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

data("BotsMemory")

BotsMemory %<>%
  recode_memory()

sim_vars <- c("sim_id", "strategy", "n_guesses", "n_players", "seed", "player_memory", "team_memory")
groupby_vars <- c(sim_vars, "inventory", "inventory_size")

BotsInventories <- BotsMemory %>%
  group_by_(.dots = sim_vars) %>%
  summarize(
    n_team_guesses = sum(n_team_guesses),
    n_player_unique_guesses = sum(n_player_unique_guesses),
    n_team_unique_guesses = sum(n_team_unique_guesses)
  ) %>%
  ungroup() %>%
  arrange(sim_id) %>%
  mutate(
    team_redundancy = 1 - n_team_unique_guesses/n_team_guesses,
    player_redundancy = 1 - n_player_unique_guesses/n_team_guesses
  ) %>%
  recode_memory()

redundancy_plot <- ggplot(BotsInventories) +
  geom_point(position = position_jitter(width = 0.2), shape = 1, size = 0.5) +
  geom_point(stat = "summary", fun.y = "mean", size = 3) +
  facet_grid(team_memory_label ~ player_memory_label)

redundancy_plot +
  aes(strategy, player_redundancy) +
  ggtitle("Player redundancy")

redundancy_plot +
  aes(strategy, team_redundancy) +
  facet_grid(player_memory_label ~ team_memory_label) +
  ggtitle("Team redundancy")
