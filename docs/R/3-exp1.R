# ---- exp1

# List to hold descriptives for in-text citation
exp1 <- list()

# Jitter Generation by TeamID for plotting
jitter_team_generation <- . %>%
  group_by(TeamID) %>%
  mutate(GenerationJittered = Generation + rnorm(1, mean = 0, sd = 0.05)) %>%
  ungroup()

# Recode Generation poly
recode_generation_quad <- . %>%
  mutate(
    GenerationSqr = Generation^2,
    Generation0Sqr = Generation0^2
  )

recode_generation_base0 <- . %>%
  mutate(Generation0 = Generation - 1)

# Methods ----
data("Sessions")

Exp1Participants <- Sessions %>%
  filter_exp1() %>%
  count(Generation) %>%
  rename(N = n) %>%
  mutate(Inheritance = c("None", rep("Diachronic", 3)))

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
  NumInnovations ~ Generation + (Generation|TeamID),
  data = Innovations
)

innovations_by_generation_quad_mod <- lmer(
  NumInnovations ~ Generation0 + Generation0Sqr + (Generation0 + Generation0Sqr|TeamID),
  data = Innovations
)
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

# Difficulty score by generation ----
data("Guesses")
data("InventoryInfo")
data("Sessions")

# Label the difficulty of each unique session result
Difficulties <- InventoryInfo %>%
  transmute(
    PrevSessionInventoryID = ID,
    UniqueSessionResult = 1,
    GuessDifficulty = (GuessDifficulty/max(GuessDifficulty)),
    CombinationDifficulty = (CombinationDifficulty/max(CombinationDifficulty))
  )

DifficultyScores <- Guesses %>%
  filter_exp1() %>%
  recode_guess_type("UniqueSessionGuess", "UniqueSessionResult") %>%
  left_join(Difficulties) %>%
  group_by(SessionID) %>%
  summarize(DifficultyScore = sum(CombinationDifficulty, na.rm = TRUE)) %>%
  left_join(Sessions) %>%
  jitter_team_generation() %>%
  recode_generation_quad()

difficulty_score_by_generation_mod <- lmer(
  DifficultyScore ~ Generation +
    (Generation|TeamID),
  data = DifficultyScores
)
difficulty_score_by_generation_preds <- data_frame(Generation = 1:4) %>%
  recode_generation_quad() %>%
  cbind(., predictSE(difficulty_score_by_generation_mod, newdata = ., SE = TRUE)) %>%
  rename(DifficultyScore = fit, SE = se.fit)

difficulty_score_by_generation_quad_mod <- lmer(
  DifficultyScore ~ Generation + GenerationSqr +
    (Generation|TeamID),
  data = DifficultyScores
)

difficulty_score_by_generation_modcomp <- 
  anova(difficulty_score_by_generation_mod, difficulty_score_by_generation_quad_mod)

difficulty_score_by_generation_plot <- ggplot(DifficultyScores) +
  aes(Generation, DifficultyScore) +
  geom_line(aes(GenerationJittered, group = TeamID),
            color = t_$color_picker("green"), alpha = 0.6) +
  geom_line(aes(group = 1), data = difficulty_score_by_generation_preds,
            color = t_$color_picker("blue"), size = 1.5) +
  geom_errorbar(aes(ymin = DifficultyScore-SE, ymax = DifficultyScore+SE),
                data = difficulty_score_by_generation_preds,
                color = t_$color_picker("blue"), width = 0.2, size = 1.5) +
  geom_point(stat = "summary", fun.y = "mean",
             color = t_$color_picker("green"), shape = 4, size = 3) +
  t_$base_theme +
  theme(
    panel.grid.minor.x = element_blank()
  )

# Page's trend test ----
# PerformanceMatrix <- Performance %>%
#   select(TeamID, Generation, NumInnovations) %>%
#   tidyr::spread(Generation, NumInnovations) %>%
#   select(-TeamID) %>%
#   as.matrix()
#
# page_trend_test_results <- crank::page.trend.test(PerformanceMatrix, ranks = FALSE)

# Inheritances ----
data("Sessions")
data("Guesses")

FinalInventoryIDs <- Guesses %>%
  filter_exp1() %>%
  group_by(SessionID) %>%
  summarize(InventoryID = PrevSessionInventoryID[SessionTime == max(SessionTime)])

AncestorMap <- Sessions %>%
  filter_exp1() %>%
  group_by(TeamID) %>%
  arrange(Generation) %>%
  mutate(AncestorID = lag(SessionID)) %>%
  ungroup() %>%
  filter(Generation > 1) %>%
  select(SessionID, AncestorID) %>%
  left_join(FinalInventoryIDs, by = c("AncestorID" = "SessionID")) %>%
  rename(AncestorInventoryID = InventoryID)

Inheritances <- Guesses %>%
  filter_exp1() %>%
  filter(Generation < 4) %>%
  group_by(SessionID) %>%
  summarize(InheritanceSize = max(SessionInventorySize) - 6) %>%
  rename(AncestorID = SessionID) %>%
  left_join(AncestorMap, .)

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

# Inherited and unique innovations ----
CostOfInheritance <- left_join(Inheritances, StageTimes) %>%
  inner_join(Innovations) %>%
  mutate(NumUniqueInnovations = NumInnovations - InheritanceSize)

innovations_created_and_inherited_plot <- ggplot(CostOfInheritance) +
  aes(InheritanceSize, NumInnovations) +
  geom_point(aes(shape = Outlier),
             position = position_jitter(width = 0.2)) +
  geom_abline(intercept = 0, slope = 1, linetype = 2, size = 0.5) +
  scale_x_continuous("Innovations inherited") +
  scale_y_continuous("Innovations created") +
  t_$scale_shape_outlier +
  t_$base_theme +
  guides(shape = "none")

unique_innovations_mod <- lmer(
  NumUniqueInnovations ~ InheritanceSize + (1|AncestorInventoryID),
  data = filter(CostOfInheritance, !Outlier)
)
unique_innovations_preds <- data_frame(InheritanceSize = 2:20) %>%
  cbind(., predictSE(unique_innovations_mod, newdata = ., se = TRUE)) %>%
  rename(NumUniqueInnovations = fit, SE = se.fit)

unique_innovations_plot <- ggplot(CostOfInheritance) +
  aes(InheritanceSize, NumUniqueInnovations) +
  geom_smooth(aes(ymin = NumUniqueInnovations-SE, ymax = NumUniqueInnovations+SE),
              stat = "identity", data = unique_innovations_preds,
              color = t_$diachronic_color) +
  geom_point(aes(shape = Outlier),
             position = position_jitter(width = 0.2)) +
  geom_hline(yintercept = 0, linetype = 2, size = 0.5) +
  scale_x_continuous("Innovations inherited") +
  scale_y_continuous("New innovations") +
  t_$scale_shape_outlier +
  guides(shape = "none") +
  t_$base_theme

# Playing time and unique innovations ----
playing_time_mod <- lmer(
  NumUniqueInnovations ~ PlayingTime + (1|AncestorInventoryID),
  data = filter(CostOfInheritance, !Outlier)
)
playing_time_preds <- data_frame(PlayingTime = 5:23) %>%
  cbind(., predictSE(playing_time_mod, newdata = ., se = TRUE)) %>%
  rename(NumUniqueInnovations = fit, SE = se.fit)

playing_time_plot <- ggplot(CostOfInheritance) +
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
  scale_x_continuous("Playing time (min)",
                     breaks = seq(-25, 25, by = 5),
                     labels = seq(-25, 25, by = 5)) +
  scale_y_continuous("Inventory Size") +
  t_$base_theme

# Guesses per item ----
data("Guesses")
data("AdjacentItems")

GuessesPerItem <- Guesses %>%
  filter_exp1() %>%
  filter(TeamID != "G47") %>%
  left_join(AdjacentItems, by = c("PrevSessionInventoryID" = "ID")) %>%
  group_by(SessionID, Adjacent) %>%
  summarize(
    TotalGuesses = n(),
    TotalTime = sum(GuessTime, na.rm = TRUE),
    Discovered = any(Result == Adjacent),
    NewItem = any(UniqueTeamResult[Result == Adjacent] == 1)
  ) %>%
  ungroup() %>%
  filter(
    Discovered == TRUE,
    NewItem == TRUE
  ) %>%
  left_join(Sessions) %>%
  label_inheritance() %>%
  recode_inheritance()

guesses_per_item_by_inheritance_mod <- lmer(
  TotalGuesses ~ Diachronic_v_NoInheritance + (Diachronic_v_NoInheritance|Adjacent) + (1|SessionID),
  data = GuessesPerItem
)
guesses_per_item_by_inheritance_preds <- recode_inheritance() %>%
  filter(Inheritance %in% c("no_inheritance", "diachronic_inheritance")) %>%
  cbind(., predictSE(guesses_per_item_by_inheritance_mod, newdata = ., se = TRUE)) %>%
  rename(TotalGuesses = fit, SE = se.fit)

guesses_per_item_by_inheritance_plot <- ggplot(GuessesPerItem) +
  aes(InheritanceLabel, TotalGuesses) +
  geom_line(aes(group = Adjacent),
            stat = "summary", fun.y = "mean",
            alpha = 0.6, color = t_$color_picker("green")) +
  geom_line(aes(group = 1),
            data = guesses_per_item_by_inheritance_preds,
            size = 2, color = t_$color_picker("blue")) +
  geom_errorbar(aes(ymin = TotalGuesses-SE, ymax = TotalGuesses+SE),
                data = guesses_per_item_by_inheritance_preds,
                width = 0.2, size = 2, color = t_$color_picker("blue")) +
  xlab("") +
  ylab("Guesses per innovation") +
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
