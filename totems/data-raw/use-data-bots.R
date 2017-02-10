library(devtools)

# Load util functions from the package.
# WARNING! Don't load the package or use_data() won't work
source("R/util.R")

assign_csvs_with_prefix("data-raw/bots", "bots_")

use_data(
  bots_replication,
  bots_strategy,
  bots_guesses,
  bots_players,
  bots_memory,
  overwrite = TRUE
)
