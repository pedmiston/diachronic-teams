source("docs/R/setup.R")

# ---- performance
data("TeamPerformance")

TeamPerformance %<>%
  filter(TeamStatus == "V") %>%
  mutate(Gap = ifelse(NumInnovations < 10, "NoAxe", "Axe"))

ggplot(TeamPerformance) +
  aes(Strategy, NumInnovations) +
  facet_grid(Gap ~ Exp) +
  geom_bar(aes(fill = Strategy), stat = "summary", fun.y = "mean", alpha = 0.2) +
  geom_point(aes(color = Strategy), position = position_jitter(width = 0.2))
