source("docs/R/setup.R")

# ---- guessing-rate
GuessingRate <- PlayerPerformance %>%
  left_join(PlayerInfo) %>%
  recode_strategy() %>%
  mutate(GuessesPerMinute = NumGuesses/SessionDuration)

guessing_rate_plot <- ggplot(GuessingRate) +
  aes(StrategyLabel, GuessesPerMinute) +
  geom_point(aes(color = StrategyLabel, group = Generation,
                 alpha = factor(Generation)),
             position = position_jitterdodge(jitter.width = 0.4)) +
  scale_y_continuous("Guesses per minute") +
  scale_alpha_manual(values = c(0.8, 0.6, 0.4)) +
  coord_cartesian(ylim = c(0, 22)) +
  guides(alpha = "none", color = "none") +
  totems_theme["scale_x_strategy"] +
  totems_theme["scale_color_strategy"] +
  totems_theme["base_theme"]
