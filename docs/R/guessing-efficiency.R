source("docs/R/setup.R")

# ---- guessing-efficiency
GuessEfficiencies <- TotemsTrials %>%
  group_by(TeamID, NumInnovations) %>%
  summarize(GuessEfficiency = 1/n()) %>%
  left_join(select(TotemsTeams, TeamID, Strategy)) %>%
  ungroup() %>%
  recode_strategy()

data("simulations_difficulty")

Simulations <- simulations_difficulty %>%
  mutate(
    NumInnovations = inventory_size - 6  # subtract starting items
  ) %>%
  select(
    SimID = sim_id,
    NumInnovations
  ) %>%
  group_by(SimID, NumInnovations) %>%
  summarize(GuessEfficiency = 1/n()) %>%
  ungroup() %>%
  group_by(NumInnovations) %>%
  summarize(GuessEfficiency = mean(GuessEfficiency))

ggplot(GuessEfficiencies) +
  aes(NumInnovations, GuessEfficiency) +
  geom_line(aes(color = StrategyLabel), stat = "summary", fun.y = "mean") +
  geom_line(data = Simulations, color = "black") +
  scale_x_continuous("Number of inventions") +
  ylab(expression(paste("Guessing rate (number of ", guesses^{-1}, ")"))) +
  totems_theme["scale_color_strategy"] +
  totems_theme["base_theme"] +
  theme(legend.position = "top")
