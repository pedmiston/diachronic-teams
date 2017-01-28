library(devtools)
library(tidyverse)

# Load functions from the package.
# WARNING! Don't load the package or use_data() won't work
source("R/util.R")

assign_csvs_with_prefix("data-raw/totems", "totems_")

totems_players <- totems_player %>%
  left_join(totems_group) %>%
  rename(Strategy = Treatment) %>%
  select(-c(ID_Number:Gain, Knowledge, Size, Open, Status)) %>%
  # Filter out any subjects that aren't reported
  # in the subject info sheet.
  inner_join(totems_subjinfo)

# Deidentification.
# Remove datetime information and shuffle rows.
set.seed(328)
id_group_levels <- factor(totems_players$ID_Group) %>% levels()
id_group_labels <- paste0("G", seq_along(id_group_levels))
totems_players %<>%
  select(-Date, -Room) %>%
  mutate(ID_Group = factor(ID_Group, levels = id_group_levels, labels = id_group_labels)) %>%
  sample_n(nrow(totems_players))

player_key <- totems_players %>%
  select(ID_Player, ID_Group, Strategy)

totems_workshops <- totems_workshop %>%
  inner_join(player_key)

use_data(
  totems_players,
  totems_workshops,
  overwrite = TRUE
)
