source("docs/R/setup.R")

# ---- attempts
Attempts <- TeamPerformance %>%
  recode_strategy()

team_attempts_mod <- lm(
  NumGuesses ~ Diachronic_v_Synchronic + Diachronic_v_Isolated,
  data = Attempts
)

team_attempts_preds <- get_lm_mod_preds(team_attempts_mod) %>%
  rename(NumGuesses = fit, SE = se.fit) %>%
  recode_strategy()

attempts_plot <- ggplot(Attempts) +
  aes(StrategyLabel, NumGuesses) +
  geom_point(aes(color = StrategyLabel),
             position = position_jitter(width = 0.4)) +
  geom_bar(aes(fill = StrategyLabel), stat = "summary", fun.y = "mean",
           alpha = 0.6) +
  geom_errorbar(aes(ymin = NumGuesses - SE, ymax = NumGuesses + SE),
                data = team_attempts_preds, width = 0.2) +
  totems_theme["scale_x_strategy"] +
  scale_y_continuous("Number of attempts") +
  totems_theme["scale_color_strategy"] +
  totems_theme["scale_fill_strategy"] +
  totems_theme["base_theme"] +
  theme(legend.position = "none")

performance_by_attempts_plot <- ggplot(Attempts) +
  aes(NumGuesses, NumInnovations, color = StrategyLabel) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous("Number of attempts") +
  scale_y_continuous("Number of inventions") +
  totems_theme["scale_color_strategy"] +
  totems_theme["base_theme"] +
  theme(legend.position = "top")
