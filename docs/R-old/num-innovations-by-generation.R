source("docs/R/setup.R")

# ---- performance-by-generation-100
data("PlayerPerformance")

PerformanceByGeneration100 <- PlayerPerformance %>%
  filter(
    TeamStatus == "V",
    Exp == "100LaborMinutes",
    Strategy != "Synchronic",
    SessionDuration == 25
  )

ggplot(PerformanceByGeneration100) +
  aes(Generation, NumInnovations) +
  geom_bar(stat = "summary", fun.y = "mean", alpha = 0.4) +
  geom_line(aes(group = TeamID), alpha = 0.4) +
  geom_line(stat = "summary", fun.y = "mean", size = 2) +
  facet_wrap("Strategy")