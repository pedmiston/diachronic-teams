source("docs/R/setup.R")

# ---- timecourse-50-teams
data("Sampled")

SampledTeamMax <- Sampled %>%
  filter(
    TeamStatus == "V",
    Exp == "50LaborMinutes"
  ) %>%
  group_by(TeamID, Strategy, SessionDuration, Generation, SampledSessionTime) %>%
  summarize(MaxInnovations = max(TeamInventorySize)) %>%
  ungroup() %>%
  recode_strategy()

ggplot(filter(SampledTeamMax, Strategy == "Synchronic")) +
  aes(SampledSessionTime, MaxInnovations, group = TeamID) +
  geom_line()

# ---- timecourse-50
data("Sampled")

SampledMeans <- Sampled %>%
  filter(
    TeamStatus == "V",
    Exp == "50LaborMinutes"
  ) %>%
  group_by(TeamID, Strategy, SessionDuration, Generation, TeamTime) %>%
  summarize(NumInnovations = mean(TeamInventorySize)) %>%
  ungroup() %>%
  recode_strategy()

ggplot(SampledMeans) +
  aes(TeamTime, NumInnovations,
      group = interaction(StrategyLabel, SessionDuration, Generation),
      color = StrategyLabel, linetype = factor(SessionDuration)) +
  geom_line(stat = "summary", fun.y = "mean")
