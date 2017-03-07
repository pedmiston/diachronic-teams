source("docs/R/setup.R")

ggplot(totems_workshops) +
  aes(TeamTime, NumUniqueGuesses, color = Strategy) +
  geom_smooth(method = "lm", se = FALSE)
