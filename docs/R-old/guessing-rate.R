source("docs/R/setup.R")

# ---- guessing-rate
GuessingRate <- PlayerPerformance %>%
  left_join(PlayerInfo) %>%
  recode_strategy() %>%
  mutate(GuessesPerMinute = NumGuesses/SessionDuration)

diachronic_generation_labels <- data_frame(
  Strategy = "Diachronic",
  Generation = 1:3,
  Label = paste0("G", Generation),
  GuessesPerMinute = 15
) %>%
  recode_strategy()

guessing_rate_plot <- ggplot(GuessingRate) +
  aes(StrategyLabel, GuessesPerMinute) +
  geom_point(aes(color = StrategyLabel, group = Generation,
                 alpha = factor(Generation)),
             position = position_jitterdodge(jitter.width = 0.4)) +
  geom_text(aes(color = StrategyLabel, group = Generation, label = Label),
            data = diachronic_generation_labels,
            position = position_dodge(width = 0.7)) +
  scale_y_continuous("Guesses per minute") +
  scale_alpha_manual(values = c(0.8, 0.6, 0.4, 0.2)) +
  coord_cartesian(ylim = c(0, 22)) +
  guides(alpha = "none", color = "none") +
  t_["scale_x_strategy"] +
  t_["scale_color_strategy"] +
  t_["base_theme"]

# Calculate guessing rate over time
SampledGuessingRate <- SampledPerformance %>%
  left_join(PlayerInfo) %>%
  filter(!(Strategy == "Diachronic" & SampledPlayerTime > 25 * 60)) %>%
  group_by(Strategy, Generation, SampledPlayerTime) %>%
  summarize(GuessesPerMinute = mean(GuessesPerMinute)) %>%
  ungroup() %>%
  recode_strategy() %>%
  recode_groups_by_generation()

guessing_rate_over_time_plot <- ggplot(SampledGuessingRate) +
  aes(SampledPlayerTime, GuessesPerMinute, color = StrategyLabel, group = GenerationStrategy) +
  geom_line() +
  scale_x_time("Player time", breaks = seconds(c(0, 25 * 60, 50 * 60))) +
  scale_y_continuous("Guesses per minute") +
  t_["scale_color_strategy"] +
  t_["base_theme"] +
  theme(legend.position = "top")
