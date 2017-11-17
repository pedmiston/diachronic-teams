source("docs/R/setup.R")

# ---- total-guesses-50

# Total guesses by strategy
data("TeamPerformance")

TeamPerformance50 <- TeamPerformance %>%
  recode_strategy() %>%
  filter(
    TeamStatus == "V",
    Exp == "50LaborMinutes",
    NumGuesses < 800
  )

total_guesses_50_mod <- lm(
  NumGuesses ~ Diachronic_v_Synchronic + Diachronic_v_Isolated + SessionDuration,
  data = TeamPerformance50
)

total_guesses_50_preds <- data_frame(
    Strategy = c("Diachronic", "Synchronic", "Isolated", "Isolated"),
    SessionDuration = c(25, 25, 25, 50)
  ) %>%
  recode_strategy() %>%
  cbind(., predict(total_guesses_50_mod, ., se = TRUE)) %>%
  rename(NumGuesses = fit, SE = se.fit) %>%
  recode_strategy()

total_guesses_50_plot <- ggplot(TeamPerformance50) +
  aes(Strategy, NumGuesses, group = interaction(Strategy, SessionDuration)) +
  geom_point(aes(color = StrategyLabel, shape = factor(SessionDuration)),
             position = position_jitterdodge(jitter.width = 0.2)) +
  geom_bar(aes(fill = StrategyLabel),
           stat = "identity", data = total_guesses_50_preds,
           position = position_dodge(width = 0.95),
           alpha = 0.6) +
  geom_errorbar(aes(ymin = NumGuesses - SE, ymax = NumGuesses + SE),
                data = total_guesses_50_preds, width = 0.2,
                position = position_dodge(width = 0.95)) +
  scale_shape_manual(values = c(16, 1)) +
  theme(legend.position = "none")

# Guesses per generation (2 x 25 minute sessions)
# Diachronic v. Isolated
data("PlayerPerformance")

PlayerPerformance50min <- PlayerPerformance %>%
  recode_strategy() %>%
  filter(
    TeamStatus == "V",
    Strategy != "Synchronic",
    SessionDuration == 25,
    Exp == "50LaborMinutes",
    NumGuesses < 400
  )

guesses_per_generation_50_mod <- lmer(
  NumGuesses ~ Generation * Diachronic_v_Isolated + (1|TeamID),
  data = PlayerPerformance50min)

guesses_per_generation_50_preds <- expand.grid(
  Generation = 1:2,
  Strategy = c("Diachronic", "Isolated"),
  stringsAsFactors = FALSE
) %>%
  recode_strategy() %>%
  cbind(., predictSE(guesses_per_generation_50_mod, newdata = ., se = TRUE)) %>%
  rename(NumGuesses = fit, SE = se.fit)

total_guesses_per_generation_50_plot <- ggplot(PlayerPerformance50min) +
  aes(Generation, NumGuesses) +
  geom_line(aes(group = TeamID), alpha = 0.6) +
  geom_line(stat = "identity", data = guesses_per_generation_50_preds,
            size = 1.5) +
  geom_errorbar(aes(ymin = NumGuesses-SE, ymax = NumGuesses+SE),
                data = guesses_per_generation_50_preds,
                width = 0.2, size = 1.5) +
  scale_x_continuous("Generation", breaks = 1:2) +
  facet_wrap("StrategyLabel")

# Num innovations by guesses
data("TeamPerformance")

TeamPerformance50 <- TeamPerformance %>%
  filter(
    Exp == "50LaborMinutes",
    TeamStatus == "V",
    NumGuesses < 800
  ) %>%
  recode_strategy()

performance_by_total_guesses_50_mod <- lm(
  NumInnovations ~ NumGuesses * (Diachronic_v_Isolated + Diachronic_v_Synchronic),
  data = TeamPerformance50
)

performance_by_total_guesses_50_preds <- expand.grid(
  NumGuesses = seq(300, 600, by = 50),
  Strategy = c("Diachronic", "Synchronic", "Isolated"),
  stringsAsFactors = FALSE
) %>%
  recode_strategy() %>%
  cbind(., predict(performance_by_total_guesses_50_mod, newdata = ., se = TRUE)) %>%
  rename(NumInnovations = fit, SE = se.fit)

performance_by_total_guesses_50_plot <- ggplot(TeamPerformance50) +
  aes(NumGuesses, NumInnovations, color = StrategyLabel) +
  geom_smooth(aes(ymin = NumInnovations-SE, ymax = NumInnovations+SE),
              stat = "identity", data = performance_by_total_guesses_50_preds) +
  geom_point() +
  theme(legend.position = "top")