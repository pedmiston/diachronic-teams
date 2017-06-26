source("docs/simulations/R/setup.R")

# ---- memory
data("BotsMemory")

BotsMemory %<>%
  recode_memory()

gg_timeline %+% BotsMemory +
  facet_grid(team_memory_label ~ player_memory_label)

gg_final %+% filter_final_round(BotsMemory) +
  facet_grid(team_memory_label ~ player_memory_label)