library(devtools)
library(tidyverse)

# Load functions from the package.
# WARNING! Don't load the package or use_data() won't work
source("R/util.R")

assign_csvs_with_prefix("data-raw/totems", "totems_")

# Player scores ----------------------------------------------------------------
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

diachronic_teams <- totems_players %>%
  filter(Strategy == "Diachronic") %>%
  # Ancestor is coded as -1 for True, meaning this player was an Ancestor
  # and 0 for False, meaning this player inherited from an Ancestor.
  # Convert this into generation, which will be 1 for all other Strategies.
  mutate(Generation = Ancestor + 2) %>%
  select(ID_Player, ID_Group, Generation)

totems_players %<>%
  left_join(diachronic_teams) %>%
  mutate(Generation = ifelse(is.na(Generation), 1, Generation))

player_key <- totems_players %>%
  select(ID_Player, ID_Group, Strategy, Generation)

totems_players %<>%
  select(ID_Player, Strategy, Generation, ID_Group, Score)

# Workshops --------------------------------------------------------------------
totems_workshops <- totems_workshop %>%
  inner_join(player_key)

# Enumerate each player's guesses
totems_workshops %<>%
  group_by(ID_Player) %>%
  mutate(GuessNumber = 1:n()) %>%
  ungroup()

# Calculate team time.
# For diachronic teams, TeamTime should reflect running total.
duration_minutes <- 25
duration_msec <- duration_minutes * 60 * 1000
totems_workshops %<>%
  left_join(player_key) %>%
  mutate(TeamTime = TrialTime + (Generation - 1) * duration_msec)

totems_workshops %<>%
  select(ID_Player, Strategy, Generation, ID_Group,
         TeamTime, GuessNumber, GuessString = WorkShopString, GuessResult = WorkShopResult,
         Inventory, InventorySize, NumAdjacent)

# Use data in package ----------------------------------------------------------
use_data(
  totems_players,
  totems_workshops,
  overwrite = TRUE
)
