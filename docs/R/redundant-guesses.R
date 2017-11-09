source("docs/R/setup.R")

# ---- redundant-guesses-50
data("TeamPerformance")

TeamPerformance50 <- TeamPerformance %>%
  recode_strategy() %>%
  filter(
    TeamStatus == "V",
    Exp == "50LaborMinutes"
  )

TeamPerformance %>%
  mutate(
    NumGuesses2 = NumRepeatedInnovations + NumRedundantGuesses + NumInnovations,
    Equal = NumGuesses == NumGuesses2
  )
