source("docs/R/setup.R")

# ---- performance-per-inventory-50
data("Inventories")

TeamInventories <- Inventories %>%
  filter(
    TeamStatus == "V",
    Exp == "50LaborMinutes"
  ) %>%
  group_by(Strategy, TeamID, SessionDuration, InventoryID) %>%
  summarize(
    TotalGuesses = sum(NumGuesses),
    TotalUniqueGuesses = sum(UniqueGuesses),
    TotalRepeatGuesses = sum(RepeatGuesses),
    TotalRepeatResults = sum(RepeatResults),
    TotalLaborTime = sum(ProblemTime)
  ) %>%
  recode_strategy()

# Total guesses per problem

total_guesses_per_problem_50_plot <- ggplot(TeamInventories) +
  aes(Strategy, TotalGuesses, group = interaction(Strategy, SessionDuration)) +
  geom_bar(aes(fill = StrategyLabel), stat = "summary", fun.y = "mean",
           position = position_dodge(width = 0.95),
           alpha = 0.6) +
  geom_point(aes(color = StrategyLabel, shape = factor(SessionDuration)),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.95)) +
  scale_shape_manual(values = c(16, 1)) +
  theme(legend.position = "none")

total_guesses_per_problem_50_mod <- lmer(
  TotalGuesses ~ Diachronic_v_Synchronic + Diachronic_v_Isolated + SessionDuration +
    (Diachronic_v_Synchronic + Diachronic_v_Isolated|InventoryID),
  data = TeamInventories
)

total_guesses_per_problem_50_preds <- data_frame(
  Strategy = c("Diachronic", "Synchronic", "Isolated", "Isolated"),
  SessionDuration = c(25, 25, 25, 50)
) %>%
  recode_strategy() %>%
  cbind(., predictSE(total_guesses_per_problem_50_mod, newdata = ., se = TRUE)) %>%
  rename(TotalGuesses = fit, SE = se.fit)

total_guesses_per_problem_50_mod_plot <- ggplot(total_guesses_per_problem_50_preds) +
  aes(Strategy, TotalGuesses, group = interaction(Strategy, SessionDuration)) +
  geom_bar(aes(fill = Strategy),
           stat = "identity", position = position_dodge(width = 0.95)) +
  geom_errorbar(aes(ymin = TotalGuesses - SE, ymax = TotalGuesses + SE),
                position = position_dodge(width = 0.95), width = 0.2)

# Total time spent per problem

total_labor_time_per_problem_50_plot <- ggplot(TeamInventories) +
  aes(Strategy, TotalLaborTime, group = interaction(Strategy, SessionDuration)) +
  geom_bar(aes(fill = StrategyLabel), stat = "summary", fun.y = "mean",
           position = position_dodge(width = 0.95),
           alpha = 0.6) +
  geom_point(aes(color = StrategyLabel, shape = factor(SessionDuration)),
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.95)) +
  scale_shape_manual(values = c(16, 1)) +
  theme(legend.position = "none")
