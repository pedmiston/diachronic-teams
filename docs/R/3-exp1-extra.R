# ---- exp1-extra

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


# Difficulty score by generation ----
data("Guesses")
data("InventoryInfo")
data("Sessions")

# Label the difficulty of each unique session result
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

difficulty_score_by_generation_mod <- lmer(
  DifficultyScore ~ Generation0 +
    (Generation0 + Generation0Sqr|TeamID),
  data = DifficultyScores
)

difficulty_score_by_generation_preds <- data_frame(Generation = 1:4) %>%
  recode_generation_base0() %>%
  recode_generation_quad() %>%
  cbind(., predictSE(difficulty_score_by_generation_mod, newdata = ., SE = TRUE)) %>%
  rename(DifficultyScore = fit, SE = se.fit)

difficulty_score_by_generation_quad_mod <- lmer(
  DifficultyScore ~ Generation0 + Generation0Sqr +
    (Generation0 + Generation0Sqr|TeamID),
  data = DifficultyScores
)

difficulty_score_by_generation_modcomp <- 
  anova(difficulty_score_by_generation_mod, difficulty_score_by_generation_quad_mod)

exp1$diff_gen0_slope_stats <- report_lmer_mod(difficulty_score_by_generation_quad_mod, "Generation0")
exp1$diff_gen0sqr_slope_stats <- report_lmer_mod(difficulty_score_by_generation_quad_mod, "Generation0Sqr")
exp1$diff_modcomp <- report_modcomp(difficulty_score_by_generation_modcomp)

# Fitting regression models to the accumulated difficulty scores revealed
# a strong linear effect, `r exp1$diff_gen0_slope_stats`, but not
# a quadratic effect, `r exp1$diff_gen0sqr_slope_stats`. Additionally,
# a model comparison revealed that the addition of the quadratic term did
# not significantly improve model fit, `r exp1$diff_modcomp`. This result
# suggests that the effect of inheritance may not have diminished
# over generations.

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
  scale_y_continuous("Accumulated difficulty score") +
  t_$base_theme +
  theme(
    panel.grid.minor.x = element_blank()
  )

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