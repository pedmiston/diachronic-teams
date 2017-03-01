library(devtools)

# Load util functions from the package.
# WARNING! Don't load the package or use_data() won't work
library(magrittr)
library(tools)
library(readr)
source("R/util.R")

read_csvs("data-raw/simulations", "simulations_")

use_data(
  simulations_replication,
  simulations_strategy,
  simulations_guesses,
  simulations_players,
  simulations_memory,
  simulations_difficulty,
  overwrite = TRUE
)
