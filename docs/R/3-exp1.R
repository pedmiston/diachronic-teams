source("docs/R/0-setup.R")
source("docs/R/2-methods.R")
# ---- exp1

# List to hold descriptives for in-text citation
exp1 <- list()

# Methods ----
data("Sessions")

Exp1Participants <- Sessions %>%
  filter_exp1() %>%
  count(Generation) %>%
  rename(N = n) %>%
  mutate(Inheritance = c("None", rep("Diachronic", 3)))

# Data ----
data("Guesses")
data("InventoryInfo")

Difficulties <- InventoryInfo %>%
  transmute(
    PrevSessionInventoryID = ID,
    UniqueSessionResult = 1,
    GuessDifficulty = (UniqueGuesses/max(UniqueGuesses)),
    CombinationDifficulty = (UniqueCombinations/max(UniqueCombinations))
  )

DifficultyScores <- Guesses %>%
  filter_exp1() %>%
  recode_guess_type("UniqueSessionGuess", "UniqueSessionResult") %>%
  left_join(Difficulties) %>%
  group_by(SessionID) %>%
  summarize(DifficultyScore = sum(CombinationDifficulty, na.rm = TRUE)) %>%
  left_join(Sessions) %>%
  jitter_team_generation() %>%
  recode_generation_base0() %>%
  recode_generation_quad()

# Page's trend test ----
data("Guesses")
data("Sessions")

Innovations <- Guesses %>%
  filter_exp1() %>%
  recode_guess_type("UniqueSessionGuess", "UniqueSessionResult") %>%
  group_by(SessionID) %>%
  summarize(NumInnovations = sum(GuessType == "unique_item")) %>%
  ungroup() %>%
  left_join(Sessions) %>%
  jitter_team_generation() %>%
  recode_generation_base0() %>%
  recode_generation_quad()

PerformanceMatrix <- Innovations %>%
  select(TeamID, Generation, NumInnovations) %>%
  tidyr::spread(Generation, NumInnovations) %>%
  select(-TeamID) %>%
  as.matrix()

page_trend_test_results <- crank::page.trend.test(PerformanceMatrix, ranks = FALSE)
page_trend_test_results$p_val_str <- compute_p_string(page_trend_test_results$px2)

exp1$page_test <- sprintf("Page's L = %.0f, $X_2$ = %.0f, %s", page_trend_test_results$L, page_trend_test_results$x2L, page_trend_test_results$p_val_str)

# Innovations by generation ----
data("Guesses")
data("Sessions")

Innovations <- Guesses %>%
  filter_exp1() %>%
  recode_guess_type("UniqueSessionGuess", "UniqueSessionResult") %>%
  group_by(SessionID) %>%
  summarize(NumInnovations = sum(GuessType == "unique_item")) %>%
  ungroup() %>%
  left_join(Sessions) %>%
  jitter_team_generation() %>%
  recode_generation_base0() %>%
  recode_generation_quad()

innovations_by_generation_mod <- lmer(
  NumInnovations ~ Generation0 + (Generation0 + Generation0Sqr|TeamID),
  data = Innovations
)
innovations_by_generation_quad_mod <- lmer(
  NumInnovations ~ Generation0 + Generation0Sqr + (Generation0 + Generation0Sqr|TeamID),
  data = Innovations
)
innovations_by_generation_modcomp <-
  anova(innovations_by_generation_mod, innovations_by_generation_quad_mod)

exp1$gen0_slope <- report_beta(innovations_by_generation_quad_mod, "Generation0")
exp1$gen0_slope_stats <- report_lmer_mod(innovations_by_generation_quad_mod, "Generation0")
exp1$gen0sqr_slope <- report_beta(innovations_by_generation_quad_mod, "Generation0Sqr")
exp1$gen0sqr_slope_stats <- report_lmer_mod(innovations_by_generation_quad_mod, "Generation0Sqr")
exp1$quad_modcomp <- report_modcomp(innovations_by_generation_modcomp)

innovations_by_generation_preds <- data_frame(Generation = 1:4) %>%
  recode_generation_base0() %>%
  recode_generation_quad() %>%
  cbind(., predictSE(innovations_by_generation_quad_mod, newdata = ., SE = TRUE)) %>%
  rename(NumInnovations = fit, SE = se.fit)

innovations_by_generation_plot <- ggplot(Innovations) +
  aes(Generation, NumInnovations) +
  geom_line(aes(GenerationJittered, group = TeamID),
            color = t_$color_picker("green"), alpha = 0.6) +
  geom_line(aes(group = 1), data = innovations_by_generation_preds,
            color = t_$color_picker("blue"), size = 1.5) +
  geom_errorbar(aes(ymin = NumInnovations-SE, ymax = NumInnovations+SE),
                data = innovations_by_generation_preds,
                color = t_$color_picker("blue"), width = 0.2, size = 1.5) +
  geom_point(stat = "summary", fun.y = "mean",
             color = t_$color_picker("green"), shape = 4, size = 3) +
  t_$scale_y_num_innovations +
  t_$base_theme +
  theme(
    panel.grid.minor.x = element_blank()
  )

# Inheritances ----
data("Sessions")
data("Guesses")

# Link each player to their ancestor
AncestorMap <- Sessions %>%
  filter_exp1() %>%
  group_by(TeamID) %>%
  arrange(Generation) %>%
  mutate(AncestorID = lag(SessionID)) %>%
  ungroup() %>%
  filter(Generation > 1) %>%
  select(SessionID, AncestorID)

# Get the final inventory id achieved by each session
InheritedInventoryIDs <- Guesses %>%
  filter_exp1() %>%
  group_by(SessionID) %>%
  summarize(AncestorInventoryID = PrevSessionInventoryID[SessionTime == max(SessionTime)]) %>%
  rename(AncestorID = SessionID)

InheritedInnovations <- Guesses %>%
  filter_exp1() %>%
  filter(Generation < 4) %>%
  group_by(SessionID) %>%
  summarize(InheritanceSize = max(SessionInventorySize) - 6) %>%
  rename(AncestorID = SessionID) %>%
  left_join(AncestorMap, .)

InheritedDifficulties <- DifficultyScores %>%
  filter(Generation < 4) %>%
  select(AncestorID = SessionID, InheritedDifficulty = DifficultyScore) %>%
  left_join(AncestorMap, .)

Inheritances <- left_join(AncestorMap, InheritedInventoryIDs) %>%
  left_join(InheritedInnovations) %>%
  left_join(InheritedDifficulties)

exp1$mean_inheritance_size <- round(mean(Inheritances$InheritanceSize), 1)
exp1$sd_inheritance_size <- round(sd(Inheritances$InheritanceSize), 1)

# Learning times ----
data("Guesses")

StageTimes <- Guesses %>%
  filter_exp1() %>%
  filter(Generation > 1) %>%
  group_by(SessionID) %>%
  summarize(LearningTime = max(SessionTime[Stage == "learning"])) %>%
  mutate(PlayingTime = 25 - LearningTime)

OutliersByLearningTime <- StageTimes %>%
  transmute(SessionID, Outlier = LearningTime > 22)
StageTimes %<>% left_join(OutliersByLearningTime)

exp1$mean_learning_time_min <- round(mean(StageTimes$LearningTime)/60, 1)
exp1$proportion_learning_time <- round((exp1$mean_learning_time_min/25) * 100, 1)

learning_times_plot <- ggplot(StageTimes) +
  aes(LearningTime, fill = Outlier) +
  geom_histogram(binwidth = 2.5, center = 1.25) +
  scale_x_continuous("Learning time (min)", breaks = seq(0, 25, by = 5)) +
  ylab("Count") +
  scale_fill_manual(values = t_$color_picker(c("blue", "orange"))) +
  t_$base_theme +
  theme(legend.position = "none")

# Learning rates ----
LearningRates <- left_join(Inheritances, StageTimes)

learning_rates_mod <- lm(LearningTime ~ InheritanceSize,
                         data = filter(LearningRates, !Outlier))
learning_rates_preds <- get_lm_mod_preds(learning_rates_mod) %>%
  rename(LearningTime = fit, SE = se.fit)

t_$scale_shape_outlier <- scale_shape_manual(values = c(1, 4))

learning_rates_plot <- ggplot(LearningRates) +
  aes(InheritanceSize, LearningTime) +
  geom_point(aes(shape = Outlier),
             position = position_jitter(width = 0.1)) +
  geom_smooth(aes(ymin = LearningTime-SE, ymax = LearningTime + SE),
              stat = "identity", data = learning_rates_preds,
              color = t_$diachronic_color) +
  scale_x_continuous("Innovations inherited") +
  scale_y_continuous("Learning time (min)", breaks = seq(0, 25, by = 5)) +
  t_$scale_shape_outlier +
  t_$base_theme +
  guides(shape = "none")

# New innovations ----
NewInnovations <- left_join(Inheritances, StageTimes) %>%
  inner_join(Innovations) %>%
  mutate(NumUniqueInnovations = NumInnovations - InheritanceSize)

innovations_created_and_inherited_plot <- ggplot(NewInnovations) +
  aes(InheritanceSize, NumInnovations) +
  geom_point(aes(shape = Outlier),
             position = position_jitter(width = 0.2)) +
  geom_abline(intercept = 0, slope = 1, linetype = 2, size = 0.5) +
  scale_x_continuous("Innovations inherited") +
  scale_y_continuous("Innovations created") +
  t_$scale_shape_outlier +
  t_$base_theme +
  guides(shape = "none")

new_innovations_mod <- lmer(
  NumUniqueInnovations ~ InheritanceSize + (1|AncestorInventoryID),
  data = filter(NewInnovations, !Outlier)
)
exp1$inheritance_size_slope_stats <- report_lmer_mod(new_innovations_mod, "InheritanceSize")

new_innovations_preds <- data_frame(InheritanceSize = 2:20) %>%
  cbind(., predictSE(new_innovations_mod, newdata = ., se = TRUE)) %>%
  rename(NumUniqueInnovations = fit, SE = se.fit)

new_innovations_plot <- ggplot(NewInnovations) +
  aes(InheritanceSize, NumUniqueInnovations) +
  geom_smooth(aes(ymin = NumUniqueInnovations-SE, ymax = NumUniqueInnovations+SE),
              stat = "identity", data = new_innovations_preds,
              color = t_$diachronic_color) +
  geom_point(aes(shape = Outlier),
             position = position_jitter(width = 0.2)) +
  geom_hline(yintercept = 0, linetype = 2, size = 0.5) +
  scale_x_continuous("Innovations inherited") +
  scale_y_continuous("New innovations") +
  t_$scale_shape_outlier +
  guides(shape = "none") +
  t_$base_theme

# Delta difficulty ----
DeltaDifficulty <- left_join(Inheritances, StageTimes) %>%
  inner_join(DifficultyScores) %>%
  mutate(DifficultyDelta = DifficultyScore - InheritedDifficulty)

delta_difficulty_mod <- lmer(
  DifficultyDelta ~ InheritanceSize + (1|AncestorInventoryID),
  data = filter(DeltaDifficulty, !Outlier)
)
exp1$delta_diff_slope_stats <- report_lmer_mod(delta_difficulty_mod, "InheritanceSize", formats = c(b=4, se=4))

delta_difficulty_preds <- data_frame(InheritanceSize = 2:20) %>%
  cbind(., predictSE(delta_difficulty_mod, newdata = ., se = TRUE)) %>%
  rename(DifficultyDelta = fit, SE = se.fit)

delta_difficulty_plot <- ggplot(DeltaDifficulty) +
  aes(InheritanceSize, DifficultyDelta) +
  geom_smooth(aes(ymin = DifficultyDelta-SE, ymax = DifficultyDelta+SE),
              stat = "identity", data = delta_difficulty_preds,
              color = t_$diachronic_color) +
  geom_point(aes(shape = Outlier),
             position = position_jitter(width = 0.2)) +
  geom_hline(yintercept = 0, linetype = 2, size = 0.5) +
  scale_x_continuous("Innovations inherited") +
  scale_y_continuous("∆ Difficulty score") +
  t_$scale_shape_outlier +
  guides(shape = "none") +
  t_$base_theme

# Playing time and unique innovations ----
playing_time_mod <- lmer(
  NumUniqueInnovations ~ PlayingTime + (1|AncestorInventoryID),
  data = filter(NewInnovations, !Outlier)
)
playing_time_preds <- data_frame(PlayingTime = 5:23) %>%
  cbind(., predictSE(playing_time_mod, newdata = ., se = TRUE)) %>%
  rename(NumUniqueInnovations = fit, SE = se.fit)

playing_time_plot <- ggplot(NewInnovations) +
  aes(PlayingTime, NumUniqueInnovations) +
  geom_smooth(aes(ymin = NumUniqueInnovations-SE, ymax = NumUniqueInnovations+SE),
              stat = "identity", data = playing_time_preds,
              color = t_$diachronic_color) +
  geom_point(aes(shape = Outlier), position = position_jitter(height = 0.1)) +
  xlab("Playing time (min)") +
  ylab("New innovations") +
  t_$scale_shape_outlier +
  guides(shape = "none") +
  t_$base_theme

# Stages ----
data("Guesses")

DiachronicInheritance <- Guesses %>%
  filter_exp1() %>%
  filter(TeamID != "G47") %>%
  filter(Generation > 1) %>%
  label_stage_time() %>%
  mutate(StageTime_2 = StageTime * StageTime)

DiachronicPlaying <- DiachronicInheritance %>%
  filter(Stage == "playing") %>%
  left_join(Inheritances)

diachronic_learning_rate_mod <- lmer(
  SessionInventorySize ~ StageTime * InheritanceSize + (StageTime|TeamID),
  data = DiachronicPlaying
)

exp1$playing_rate_beta <- report_beta(diachronic_learning_rate_mod, "StageTime")
exp1$playing_rate_slope <- report_lmer_mod(diachronic_learning_rate_mod, "StageTime")
exp1$playing_rate_inter <- report_lmer_mod(diachronic_learning_rate_mod, "StageTime:InheritanceSize", formats = c(b=3, se = 3, t = 1))

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
  scale_x_continuous("Playing time (min)",
                     breaks = seq(-25, 25, by = 5),
                     labels = seq(-25, 25, by = 5)) +
  scale_y_continuous("Inventory Size") +
  t_$base_theme



# Guess types ----
data("Guesses")
data("Sessions")

Guesses %<>%
  filter_exp1() %>%
  filter(TeamID != "G47") %>%
  recode_guess_type("UniqueSessionGuess", "UniqueSessionResult") %>%
  group_by(SessionID) %>%
  count(GuessType) %>%
  mutate(PropGuesses = n/sum(n)) %>%
  ungroup() %>%
  rename(NumGuesses = n) %>%
  left_join(Sessions) %>%
  label_inheritance() %>%
  recode_inheritance()

RedundantGuesses <- filter(Guesses, GuessType == "redundant")
redundant_guesses_mod <- lm(NumGuesses ~ Diachronic_v_NoInheritance,
                            data = RedundantGuesses)
prop_redundant_guesses_mod <- lm(PropGuesses ~ Diachronic_v_NoInheritance,
                                 data = RedundantGuesses)

UniqueGuesses <- Guesses %>% filter(GuessType == "unique_guess")
unique_guesses_mod <- lm(NumGuesses ~ Diachronic_v_NoInheritance,
                         data = UniqueGuesses)
prop_unique_guesses_mod <- lm(PropGuesses ~ Diachronic_v_NoInheritance,
                              data = UniqueGuesses)

RepeatItems <- Guesses %>% filter(GuessType == "repeat_item")
repeat_items_mod <- lm(NumGuesses ~ Diachronic_v_NoInheritance,
                       data = RepeatItems)
prop_repeat_items_mod <- lm(NumGuesses ~ Diachronic_v_NoInheritance,
                            data = RepeatItems)

GuessTypesProp <- Guesses %>%
  group_by(Diachronic_v_NoInheritance, GuessType) %>%
  summarize(
    PropGuesses = mean(PropGuesses)
  ) %>%
  ungroup() %>%
  left_join(
    filter(recode_inheritance(), Inheritance != "individual_inheritance")
  ) %>%
  left_join(
    recode_guess_type()
  )

guess_types_prop_plot <- ggplot(GuessTypesProp) +
  aes(InheritanceLabel, PropGuesses) +
  geom_bar(aes(fill = GuessTypeLabel), stat = "identity") +
  xlab("") +
  scale_y_continuous("Guesses", labels = scales::percent) +
  scale_fill_manual("Guess type",
                    values = t_$color_picker(c("green", "blue", "orange", "pink"))) +
  t_$base_theme
