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

# redundancy

