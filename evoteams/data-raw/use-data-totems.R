library(devtools)
library(tidyverse)

# Load functions from the package.
# WARNING! Don't load the package or use_data() won't work
source("R/util.R")

assign_csvs_with_prefix("data-raw/totems", "totems_")

totems_leaderboards <- totems_player %>%
  left_join(totems_group) %>%
  rename(Strategy = Treatment) %>%
  select(-c(ID_Number:Gain, Knowledge, Size, Open, Status)) %>%
  # Filter out any subjects that aren't reported
  # in the subject info sheet.
  inner_join(totems_subjinfo)

# Deidentification.
# Remove datetime information and shuffle rows.
set.seed(328)
totems_leaderboards %<>%
  select(-Date, -Room) %>%
  sample_n(nrow(totems_leaderboards))

use_data(
  totems_leaderboards,
  overwrite = TRUE
)
