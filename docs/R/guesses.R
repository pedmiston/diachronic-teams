source("docs/R/setup.R")

# ---- guesses
data("TeamPerformance")

TeamPerformance %<>%
  recode_strategy() %>%
  filter(
    TeamStatus == "V",
    Exp == "50LaborMinutes"
  )

guesses_mod <- lm(
  NumGuesses ~ Diachronic_v_Synchronic + Diachronic_v_Isolated + SessionDurationLabel,
  data = TeamPerformance
)

guesses_preds <- recode_strategy() %>%
  cbind(., predict(guesses_mod, ., se = TRUE)) %>%
  rename(NumGuesses = fit, SE = se.fit) %>%
  recode_strategy()

guesses_plot <- ggplot(TeamPerformance) +
  aes(StrategyLabel, NumGuesses) +
  geom_point(aes(color = StrategyLabel),
             position = position_jitter(width = 0.2)) +
  geom_bar(aes(fill = StrategyLabel), stat = "summary", fun.y = "mean",
           alpha = 0.6) +
  geom_errorbar(aes(ymin = NumGuesses - SE, ymax = NumGuesses + SE),
                data = guesses_preds, width = 0.2) +
  theme(legend.position = "none")
guesses_plot

# ---- performance-by-guesses
performance_by_guesses_plot <- ggplot(TeamPerformance) +
  aes(NumGuesses, NumInnovations, color = StrategyLabel) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme(legend.position = "top")
performance_by_guesses_plot

# ---- player-guesses
data("PlayerPerformance")

PlayerPerformance %<>%
  filter(TeamStatus == "V") %>%
  recode_experiment()

ggplot(PlayerPerformance) +
  aes(Strategy, NumGuesses) +
  geom_bar(stat = "summary", fun.y = "mean") +
  geom_point() +
  facet_wrap("ExpLabel")
