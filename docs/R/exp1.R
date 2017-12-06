source("docs/R/_setup.R")

# ---- exp1

# List to hold descriptives for in-text citation
exp1 <- list()

# Inheritances ----
data("Sessions")
data("Guesses")

Sessions %<>% filter(TeamID != "G47")
Guesses %<>% filter(TeamID != "G47")

AncestorMap <- Sessions %>%
  filter_diachronic_exp() %>%
  group_by(TeamID) %>%
  arrange(Generation) %>%
  mutate(AncestorID = lag(SessionID)) %>%
  ungroup() %>%
  filter(Generation > 1) %>%
  select(SessionID, AncestorID)

Inheritances <- Guesses %>%
  filter_diachronic_exp() %>%
  filter(Generation < 4) %>%
  group_by(SessionID) %>%
  summarize(InheritanceSize = max(SessionInventorySize) - 6) %>%
  rename(AncestorID = SessionID) %>%
  left_join(AncestorMap, .)

exp1$mean_inheritance_size <- round(mean(Inheritances$InheritanceSize), 1)
exp1$sd_inheritance_size <- round(sd(Inheritances$InheritanceSize), 1)

# Learning times ----
data("Guesses")

Guesses %<>% filter(TeamID != "G47")

LearningTimes <- Guesses %>%
  filter_diachronic_exp() %>%
  filter(Generation > 1) %>%
  group_by(SessionID) %>%
  summarize(EndLearning = max(SessionTime[Stage == "learning"]))

exp1$mean_learning_time_min <- round(mean(LearningTimes$EndLearning)/60, 1)
exp1$proportion_learning_time <- round((exp1$mean_learning_time_min/25) * 100, 1)

learning_times_plot <- ggplot(LearningTimes) +
  aes(EndLearning) +
  geom_histogram(binwidth = 2.5, center = 1.25) +
  scale_x_continuous("Time of learning stage", breaks = seq(0, 25, by = 5))

# Learning rates ----
LearningRates <- left_join(Inheritances, LearningTimes)

learning_rates_mod <- lm(EndLearning ~ InheritanceSize, data = LearningRates)
learning_rates_preds <- get_lm_mod_preds(learning_rates_mod) %>%
  rename(EndLearning = fit, SE = se.fit)

learning_rates_plot <- ggplot(LearningRates) +
  aes(InheritanceSize, EndLearning) +
  geom_point(position = position_jitter(width = 0.1)) +
  geom_smooth(aes(ymin = EndLearning-SE, ymax = EndLearning + SE),
              stat = "identity", data = learning_rates_preds) +
  scale_x_continuous("Number of items inherited") +
  scale_y_continuous("Time of learning stage (min)", breaks = seq(0, 25, by = 5))

# Stages ----
data("Guesses")

Guesses %<>% filter(TeamID != "G47")

DiachronicInheritance <- Guesses %>%
  filter_diachronic_exp() %>%
  filter(Generation > 1) %>%
  label_stage_time() %>%
  mutate(StageTime_2 = StageTime * StageTime)

diachronic_player_stages_plot <- ggplot(DiachronicInheritance) +
  aes(SessionTime, SessionInventorySize) +
  geom_line(aes(group = SessionID), color = t_$color_picker("green")) +
  theme(legend.position = "none")

illustrative_sessions <- c("G123", "G160", "G164")
illustrative_diachronic_player_stages_plot <- (
  # Replace data in plot with just the data from the illustrative teams
  diachronic_player_stages_plot %+% dplyr::filter(DG2, SessionID %in% illustrative_teams)
) +
  geom_vline(aes(xintercept = EndLearning),
             data = dplyr::filter(DG2Stages, TeamID %in% illustrative_teams),
             color = t_$color_picker("blue")) +
  scale_x_continuous("Session time (min)",
                     breaks = seq(0, 25 * 60, by = 5 * 60),
                     labels = seq(0, 25,      by = 5)) +
  scale_y_continuous("Inventory Size") +
  t_$base_theme +
  theme(legend.position = "none")

DiachronicPlaying <- DiachronicInheritance %>%
  filter(Stage == "playing") %>%
  left_join(Inheritances)

diachronic_learning_rate_mod <- lmer(
  SessionInventorySize ~ (StageTime + StageTime_2) * InheritanceSize + (0 + StageTime + StageTime_2|TeamID),
  data = DiachronicPlaying
)

mean_inheritance_size <- mean(Inheritances$InheritanceSize)
sd_inheritance_size <- sd(Inheritances$InheritanceSize)
sampled_inheritance_sizes <- c(mean_inheritance_size - sd_inheritance_size, mean_inheritance_size, mean_inheritance_size + sd_inheritance_size)

diachronic_learning_rate_preds <- expand.grid(
  StageTime = seq(0, 20, by = 1),
  InheritanceSize = sampled_inheritance_sizes
) %>%
  mutate(StageTime_2 = StageTime * StageTime) %>%
  cbind(., predictSE(diachronic_learning_rate_mod, newdata = ., se = TRUE)) %>%
  rename(SessionInventorySize = fit, SE = se.fit)

diachronic_player_trajectories_plot <- ggplot(DiachronicInheritance) +
  aes(StageTime, SessionInventorySize) +
  geom_line(aes(group = SessionID), color = t_$color_picker("green")) +
  geom_vline(xintercept = 0, color = t_$color_picker("blue")) +
  geom_smooth(aes(ymin = SessionInventorySize - SE, ymax = SessionInventorySize + SE, group = InheritanceSize),
              stat = "identity", data = diachronic_learning_rate_preds,
              color = t_$color_picker("orange")) +
  scale_x_continuous("Playin time (min)",
                     breaks = seq(-25, 25, by = 5),
                     labels = seq(-25, 25, by = 5)) +
  scale_y_continuous("Inventory Size") +
  t_$base_theme

# Performance rel player ----
data("DiachronicPerformance")

DiachronicPerformance %<>%
  filter(GuessesRel == "player_guesses", GuessType == "unique_item") %>%
  rename(NumInnovations = NumGuesses)

ggplot(DiachronicPerformance) +
  aes(Generation, NumInnovations) +
  geom_line(aes(group = TeamID))

# Performance rel team ----
data("DiachronicPerformance")

DiachronicPerformance %<>%
  filter(Generation > 1) %>%
  left_join(Inheritances) %>%
  filter(GuessesRel == "team_guesses", GuessType == "unique_item") %>%
  rename(NumInnovations = NumGuesses)

ggplot(DiachronicPerformance) +
  aes(InheritanceSize, NumInnovations) +
  geom_point(position = position_jitter(width = 0.2))
