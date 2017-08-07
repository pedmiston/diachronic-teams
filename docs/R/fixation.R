source("docs/R/setup.R")

# ---- fixation
data("Guesses")

IncorrectGuesses <- Guesses %>%
  filter(Result == 0) %>%
  count(Guess) %>%
  rename(NumGuesses = n) %>%
  mutate(
    PctIncorrectGuesses = 100 * (NumGuesses/sum(NumGuesses)),
    GuessesPerPlayer = NumGuesses/length(unique(Guesses$PlayerID))
  ) %>%
  arrange(desc(PctIncorrectGuesses))

GuessRedundancyLevel <- IncorrectGuesses$Guess
IncorrectGuesses %<>%
  mutate(
    RankedGuesses = factor(Guess, levels = GuessRedundancyLevel),
    RankedGuessesRev = factor(Guess, levels = rev(GuessRedundancyLevel))
  )

Top10IncorrectGuesses <- IncorrectGuesses %>%
  filter(Guess %in% GuessRedundancyLevel[1:10])

Top10IncorrectGuessesPlot <- ggplot(Top10IncorrectGuesses) +
  aes(RankedGuessesRev, GuessesPerPlayer) +
  geom_bar(stat = "identity") +
  coord_flip()

# Top10IncorrectGuessesImages <- Top10IncorrectGuesses %>%
#   rowwise() %>%
#   do(GuessImage = view_guess(.$Guess)) %>%
#   ungroup() %>%
#   mutate(GuessRank = 1:n())
# 
# ggplot(Top10IncorrectGuesses) +
#   aes(x = GuessRank, y = 0) +
#   annotation_custom(Top)


# How well do people correlate for incorrect guesses?
#
# For each Player, tally the number of times each incorrect
# guess was made
# IncorrectGuessesByPlayer <- Guesses %>%
#   filter(Result == 0) %>%
#   count(PlayerID, Guess) %>%
#   rename(NumGuesses = n) %>%
#   mutate(NumRedundantGuesses = NumGuesses - 1) %>%
#   arrange(PlayerID, desc(NumRedundantGuesses)) %>%
#   mutate(FixationRank = rank(-NumRedundantGuesses, ties.method = "min")) %>%
#   ungroup()
# 
# IncorrectGuessesByPlayer %<>%
#   left_join(PlayerInfo) %>%
#   left_join(TeamInfo) %>%
#   recode_strategy()

# Estimate correlation among Isolated players
# IncorrectGuessesByIsolatedPlayers <- IncorrectGuessesByPlayer %>%
#   left_join(PlayerInfo) %>%
#   filter(Strategy == "Isolated") %>%
#   select(Guess, PlayerID, NumRedundantGuesses) %>%
#   spread(PlayerID, NumRedundantGuesses)
# Dead end because the data is too sparse!

# Test whether there are differences in the "fixedness" of various
# incorrect guesses by strategy.
# redundancy_by_strategy_mod <- glmer(
#   NumRedundantGuesses ~ DvS + DSvI +
#     (1|Guess) + (1|TeamID),
#   family = "poisson", data = IncorrectGuessesByPlayer)
# summary(redundancy_by_strategy_mod)
# Dead end because model doesn't converge.
