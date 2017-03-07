source("docs/R/setup.R")


ggplot(totems_workshops) +
  aes(TeamTime, NumUniqueGuesses, color = Strategy) +
  geom_point(stat = "summary", fun.y = "mean") +
  geom_smooth(method = "lm", se = FALSE)
