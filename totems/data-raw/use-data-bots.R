library(devtools)

# Load util functions from the package.
# WARNING! Don't load the package or use_data() won't work
source("R/util.R")

assign_csvs_with_prefix("data-raw/simulations", "simulations_")

use_data(
  simulations_replication,
  simulations_strategy,
  simulations_guesses,
  simulations_players,
  simulations_memory,
  simulations_difficulty,
  overwrite = TRUE
)
