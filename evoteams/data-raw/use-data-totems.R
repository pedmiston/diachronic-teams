library(devtools)
library(tidyverse)

# Load functions from the package.
# WARNING! Don't load the package or use_data() won't work
source("R/util.R")

assign_csvs_with_prefix("data-raw/totems", "totems_")

totems_leaderboards <- totems_player %>%
  left_join(totems_group) %>%
  rename(Strategy = Treatment) %>%
  inner_join(totems_subjinfo)

use_data(
  totems_leaderboards,
  overwrite = TRUE
)
