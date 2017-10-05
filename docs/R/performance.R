source("docs/R/setup.R")

# ---- performance
data("TeamPerformance")

ggplot(TeamPerformance) +
  aes(Strategy, UniqueTeamItems) +
  facet_wrap("Exp") +
  geom_bar(aes(fill = Strategy), stat = "summary", fun.y = "mean") +
  geom_point(aes(color = Strategy), position = position_jitter(width = 0.2))
