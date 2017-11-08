source("docs/R/setup.R")

# ---- guesses-50
data("TeamPerformance")

TeamPerformance %<>%
  recode_strategy() %>%
  filter(
    TeamStatus == "V",
    Exp == "50LaborMinutes",
    SessionDuration == 25
  )

guesses_mod <- lm(
  NumGuesses ~ Diachronic_v_Synchronic + Diachronic_v_Isolated,
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

# ---- guesses-50-generation
data("PlayerPerformance")

PlayerPerformance %<>%
  recode_strategy() %>%
  filter(
    TeamStatus == "V",
    Strategy != "Synchronic",
    SessionDuration == 25,
    Exp == "50LaborMinutes"
  )

guesses_per_generation_mod <- lmer(
  NumGuesses ~ Generation * Diachronic_v_Isolated + (1|TeamID),
  data = PlayerPerformance)

guesses_per_generation_preds <- expand.grid(
    Generation = 1:2,
    Strategy = c("Diachronic", "Isolated"),
    stringsAsFactors = FALSE
  ) %>%
  recode_strategy() %>%
  cbind(., predictSE(guesses_per_generation_mod, newdata = ., se = TRUE)) %>%
  rename(NumGuesses = fit, SE = se.fit)

ggplot(PlayerPerformance) +
  aes(Generation, NumGuesses, group = Strategy) +
  geom_bar(aes(fill = Strategy), stat = "identity",
           position = position_dodge(0.9), alpha = 0.6,
           data = guesses_per_generation_preds) +
  geom_errorbar(aes(ymin = NumGuesses-SE, ymax = NumGuesses+SE),
                position = position_dodge(width = 0.9), width = 0.2,
                data = guesses_per_generation_preds) +
  geom_point(aes(color = Strategy),
             position = position_jitterdodge(jitter.width = 0.2)) +
  scale_x_continuous("Generation", breaks = 1:2)

# ---- guesses-50-session-duration
data("TeamPerformance")

Isolated50 <- TeamPerformance %>%
  filter(
    Strategy == "Isolated",
    Exp == "50LaborMinutes",
    TeamStatus == "V"
  )

isolated50_guesses_mod <- lm(NumGuesses ~ SessionDuration, data = Isolated50)
isolated50_guesses_preds <- data_frame(SessionDuration = c(25, 50)) %>%
  cbind(., predict(isolated50_guesses_mod, newdata = ., se = TRUE)) %>%
  rename(NumGuesses = fit, SE = se.fit)

ggplot(Isolated50) +
  aes(factor(SessionDuration), NumGuesses) +
  geom_bar(stat = "identity", data = isolated50_guesses_preds) +
  geom_errorbar(aes(ymin = NumGuesses-SE, ymax = NumGuesses+SE),
                width = 0.2,
                data = isolated50_guesses_preds) +
  geom_point(position = position_jitter(width = 0.2))

# ---- guesses-50-performance
data("TeamPerformance")

TeamPerformance <- TeamPerformance %>%
  filter(
    Exp == "50LaborMinutes",
    TeamStatus == "V"
  ) %>%
  recode_strategy()

guesses50_performance_mod <- lm(
  NumInnovations ~ NumGuesses * (Diachronic_v_Isolated + Diachronic_v_Synchronic),
  data = TeamPerformance
)

guesses50_performance_preds <- expand.grid(
    NumGuesses = seq(300, 600, by = 50),
    Strategy = c("Diachronic", "Synchronic", "Isolated"),
    stringsAsFactors = FALSE
  ) %>%
  recode_strategy() %>%
  cbind(., predict(guesses50_performance_mod, newdata = ., se = TRUE)) %>%
  rename(NumInnovations = fit, SE = se.fit)

performance_by_guesses_plot <- ggplot(TeamPerformance) +
  aes(NumGuesses, NumInnovations, color = StrategyLabel) +
  geom_smooth(aes(ymin = NumInnovations-SE, ymax = NumInnovations+SE),
              stat = "identity", data = guesses50_performance_preds) +
  geom_point() +
  theme(legend.position = "top")
performance_by_guesses_plot

# ---- unique-guesses-50
data("TeamPerformance")

TeamPerformance %<>%
  recode_strategy() %>%
  filter(
    TeamStatus == "V",
    Exp == "50LaborMinutes",
    SessionDuration == 25
  )

unique_guesses_mod <- lm(
  NumUniqueGuesses ~ Diachronic_v_Synchronic + Diachronic_v_Isolated,
  data = TeamPerformance
)

unique_guesses_preds <- recode_strategy() %>%
  cbind(., predict(unique_guesses_mod, ., se = TRUE)) %>%
  rename(NumUniqueGuesses = fit, SE = se.fit) %>%
  recode_strategy()

guesses_plot <- ggplot(TeamPerformance) +
  aes(StrategyLabel, NumUniqueGuesses) +
  geom_point(aes(color = StrategyLabel),
             position = position_jitter(width = 0.2)) +
  geom_bar(aes(fill = StrategyLabel),
           stat = "identity", alpha = 0.6,
           data = unique_guesses_preds) +
  geom_errorbar(aes(ymin = NumUniqueGuesses - SE, ymax = NumUniqueGuesses + SE),
                data = unique_guesses_preds, width = 0.2) +
  theme(legend.position = "none")
guesses_plot

# ---- unique-guesses-50-performance
data("TeamPerformance")

TeamPerformance <- TeamPerformance %>%
  filter(
    Exp == "50LaborMinutes",
    TeamStatus == "V"
  ) %>%
  recode_strategy()

guesses50_performance_mod <- lm(
  NumInnovations ~ NumUniqueGuesses * (Diachronic_v_Isolated + Diachronic_v_Synchronic),
  data = TeamPerformance
)

guesses50_performance_preds <- expand.grid(
  NumUniqueGuesses = seq(100, 300, by = 50),
  Strategy = c("Diachronic", "Synchronic", "Isolated"),
  stringsAsFactors = FALSE
) %>%
  recode_strategy() %>%
  cbind(., predict(guesses50_performance_mod, newdata = ., se = TRUE)) %>%
  rename(NumInnovations = fit, SE = se.fit)

performance_by_guesses_plot <- ggplot(TeamPerformance) +
  aes(NumUniqueGuesses, NumInnovations, color = StrategyLabel) +
  geom_smooth(aes(ymin = NumInnovations-SE, ymax = NumInnovations+SE),
              stat = "identity", data = guesses50_performance_preds) +
  geom_point() +
  theme(legend.position = "top")
performance_by_guesses_plot
