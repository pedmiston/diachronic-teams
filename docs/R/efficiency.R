source("docs/R/setup.R")

ggplot(totems_workshops) +
  aes(TeamTime, NumUniqueGuesses, color = Strategy) +
  geom_smooth(aes(group = Strategy_Group_Time), method = "loess", se = FALSE)

ggplot(totems_workshops) +
  aes(TeamTime, TeamNumUniqueGuesses, color = Strategy) +
  geom_smooth(aes(group = Strategy_Group_Time), method = "loess", se = FALSE)
