# ---- unique-guesses-50

# Unique guesses by strategy
data("TeamPerformance")

TeamPerformance50 <- TeamPerformance %>%
  recode_strategy() %>%
  filter(
    TeamStatus == "V",
    Exp == "50LaborMinutes",
    NumGuesses < 800
  )

unique_guesses_50_mod <- lm(
  NumUniqueGuesses ~ Diachronic_v_Synchronic + Diachronic_v_Isolated + SessionDuration,
  data = TeamPerformance50
)

unique_guesses_50_preds <- data_frame(
    Strategy = c("Diachronic", "Synchronic", "Isolated", "Isolated"),
    SessionDuration = c(25, 25, 25, 50)
  ) %>%
  recode_strategy() %>%
  cbind(., predict(unique_guesses_50_mod, ., se = TRUE)) %>%
  rename(NumUniqueGuesses = fit, SE = se.fit) %>%
  recode_strategy()

unique_guesses_50_plot <- ggplot(TeamPerformance50) +
  aes(Strategy, NumUniqueGuesses, group = interaction(Strategy, SessionDuration)) +
  geom_point(aes(color = StrategyLabel, shape = factor(SessionDuration)),
             position = position_jitterdodge(jitter.width = 0.2)) +
  geom_bar(aes(fill = StrategyLabel),
           stat = "identity", data = unique_guesses_50_preds,
           position = position_dodge(width = 0.95),
           alpha = 0.6) +
  geom_errorbar(aes(ymin = NumUniqueGuesses - SE, ymax = NumUniqueGuesses + SE),
                data = unique_guesses_50_preds, width = 0.2,
                position = position_dodge(width = 0.95)) +
  scale_shape_manual(values = c(16, 1)) +
  theme(legend.position = "none")

# Unique guesses controlling for total guesses
unique_guesses_by_total_guesses_50_mod <- lm(
  NumUniqueGuesses ~ NumGuesses * (Diachronic_v_Isolated + Diachronic_v_Synchronic) + SessionDuration,
  data = TeamPerformance50
)

unique_guesses_by_total_guesses_50_preds <- expand.grid(
  NumGuesses = seq(200, 600, by = 50),
  Strategy = c("Diachronic", "Synchronic", "Isolated"),
  SessionDuration = c(25, 50),
  stringsAsFactors = FALSE
) %>%
  filter(!(Strategy != "Isolated" & SessionDuration == 50)) %>%
  recode_strategy() %>%
  cbind(., predict(unique_guesses_by_total_guesses_50_mod, newdata = ., se = TRUE)) %>%
  rename(NumUniqueGuesses = fit, SE = se.fit)

unique_guesses_by_total_guesses_50_plot <- ggplot(TeamPerformance50) +
  aes(NumGuesses, NumUniqueGuesses,
      group = interaction(Strategy, SessionDuration),
      color = Strategy) +
  geom_point(aes(color = StrategyLabel, shape = factor(SessionDuration))) +
  geom_smooth(aes(linetype = factor(SessionDuration),
                  ymin = NumUniqueGuesses - SE,
                  ymax = NumUniqueGuesses + SE),
              data = unique_guesses_by_total_guesses_50_preds,
              stat = "identity") +
  scale_shape_manual(values = c(16, 1)) +
  theme(legend.position = "none")

# Unique guesses by generation
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

unique_guesses_per_generation_50_mod <- lmer(
  NumUniqueGuesses ~ Generation * Diachronic_v_Isolated + (1|TeamID),
  data = PlayerPerformance50min)

unique_guesses_per_generation_50_preds <- expand.grid(
  Generation = 1:2,
  Strategy = c("Diachronic", "Isolated"),
  stringsAsFactors = FALSE
) %>%
  recode_strategy() %>%
  cbind(., predictSE(unique_guesses_per_generation_50_mod, newdata = ., se = TRUE)) %>%
  rename(NumUniqueGuesses = fit, SE = se.fit)

unique_guesses_per_generation_50_plot <- ggplot(PlayerPerformance50min) +
  aes(Generation, NumUniqueGuesses) +
  geom_line(aes(group = TeamID), alpha = 0.6) +
  geom_line(stat = "identity", data = unique_guesses_per_generation_50_preds,
            size = 1.5) +
  geom_errorbar(aes(ymin = NumUniqueGuesses-SE, ymax = NumUniqueGuesses+SE),
                data = unique_guesses_per_generation_50_preds,
                width = 0.2, size = 1.5) +
  scale_x_continuous("Generation", breaks = 1:2) +
  facet_wrap("StrategyLabel")

# Num innovations by unique guesses
data("TeamPerformance")

TeamPerformance50 <- TeamPerformance %>%
  filter(
    Exp == "50LaborMinutes",
    TeamStatus == "V",
    NumGuesses < 800
  ) %>%
  recode_strategy()

num_innovations_by_unique_guesses_mod <- lm(
  NumInnovations ~ NumUniqueGuesses * (DvS + DSvI) + SessionDuration,
  data = TeamPerformance50
)

num_innovations_by_unique_guesses_preds <- expand.grid(
  NumUniqueGuesses = seq(100, 300, by = 50),
  Strategy = c("Diachronic", "Synchronic", "Isolated"),
  SessionDuration = c(25, 50),
  stringsAsFactors = FALSE
) %>%
  filter(!(Strategy != "Isolated" & SessionDuration == 50)) %>%
  recode_strategy() %>%
  cbind(., predict(num_innovations_by_unique_guesses_mod, newdata = ., se = TRUE)) %>%
  rename(NumInnovations = fit, SE = se.fit)

num_innovations_by_unique_guesses_50_plot <- ggplot(TeamPerformance50) +
  aes(NumUniqueGuesses, NumInnovations, 
      group = interaction(Strategy, SessionDuration),
      color = StrategyLabel) +
  geom_smooth(aes(ymin = NumInnovations-SE, ymax = NumInnovations+SE,
                  linetype = factor(SessionDuration)),
              stat = "identity", data = num_innovations_by_unique_guesses_preds) +
  geom_point(aes(shape = factor(SessionDuration))) +
  scale_shape_manual(values = c(16, 1)) +
  theme(legend.position = "top")
