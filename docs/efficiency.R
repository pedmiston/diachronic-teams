source("docs/R/setup.R")

totems_workshops %>%
  group_by(ID_Player, InventorySize) %>%
  summarize(
    Guesses = n(),
    UniqueGuesses = sum(UniqueGuess),
    TeamUniqueGuesses = sum(TeamUniqueGuess)
  )
