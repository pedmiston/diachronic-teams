source("docs/R/setup.R")

# ---- timecourse-50
data("Sampled")

SampledMeans <- Sampled %>%
  filter(
    TeamStatus == "V",
    SessionStatus == "V",
    Exp == "50LaborMinutes"
  ) %>%
  group_by(TeamID, Strategy, SessionDuration, Generation, SampledSessionTime) %>%
  summarize(NumInnovations = mean(TeamInventorySize)) %>%
  ungroup() %>%
  recode_strategy()

ggplot(SampledMeans) +
  aes(SampledSessionTime, NumInnovations,
      group = interaction(StrategyLabel, SessionDuration),
      color = StrategyLabel, linetype = factor(SessionDuration)) +
  geom_line(stat = "summary", fun.y = "mean")
