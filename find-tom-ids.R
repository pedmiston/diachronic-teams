# Find TOM IDs to use as starting points for new diachronic subjects.
library(tidyverse)
library(totems)
data("Sessions")

valid_tom_player_ids <- Sessions %>%
  filter_selfother() %>%
  filter(Strategy == "Isolated") %>%
  count(PlayerID) %>%
  filter(n == 4) %>%
  .$PlayerID

n_valid_tom <- length(valid_tom_player_ids)

new_ids <- Sessions %>%
  filter(
    PlayerID %in% valid_tom_player_ids,
    SessionIX < 4
  ) %>%
  mutate(AncestorID = as.numeric(str_replace(SessionID, "S", ""))) %>%
  arrange(SessionIX) %>%
  filter(SessionIX > 1)
  .$AncestorID

write(paste(new_ids, collapse = "\n"), "tom-ids.txt")
