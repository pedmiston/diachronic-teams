source("docs/R/0-setup.R")
# ---- exp1

# List to hold descriptives for in-text citation
exp1 <- list()

# Innovations by generation ----
data("Guesses")
data("Sessions")

Guesses %<>%
  filter_diachronic_exp() %>%
  filter(TeamID != "G47") %>%
  recode_guess_type("UniqueSessionGuess", "UniqueSessionResult") %>%
  group_by(SessionID) %>%
  count(GuessType) %>%
  mutate(PropGuesses = n/sum(n)) %>%
  ungroup() %>%
  rename(NumGuesses = n) %>%
  left_join(Sessions)

Innovations <- Guesses %>%
  filter(GuessType == "unique_item") %>%
  rename(NumInnovations = NumGuesses) %>%
  group_by(TeamID) %>%
  mutate(GenerationJittered = Generation + rnorm(1, mean = 0, sd = 0.05)) %>%
  ungroup()

innovations_by_generation_mod <- lmer(
  NumInnovations ~ Generation + (Generation|TeamID),
  data = Innovations
)
innovations_by_generation_preds <- data_frame(Generation = 1:4) %>%
  cbind(., predictSE(innovations_by_generation_mod, newdata = ., SE = TRUE)) %>%
  rename(NumInnovations = fit, SE = se.fit)

innovations_by_generation_plot <- ggplot(Innovations) +
  aes(Generation, NumInnovations) +
  geom_line(aes(GenerationJittered, group = TeamID),
            color = t_$color_picker("green"), alpha = 0.6) +
  geom_line(aes(group = 1), data = diachronic_performance_by_generation_preds,
            color = t_$color_picker("blue"), size = 1.5) +
  geom_errorbar(aes(ymin = NumInnovations-SE, ymax = NumInnovations+SE),
                data = diachronic_performance_by_generation_preds,
                color = t_$color_picker("blue"), width = 0.2, size = 1.5) +
  geom_point(stat = "summary", fun.y = "mean",
             color = t_$color_picker("green"), size = 3) +
  t_$scale_y_num_innovations +
  t_$base_theme +
  theme(
    panel.grid.minor.x = element_blank()
  )

# Page's trend test ----
PerformanceMatrix <- Performance %>%
  select(TeamID, Generation, NumInnovations) %>%
  tidyr::spread(Generation, NumInnovations) %>%
  select(-TeamID) %>%
  as.matrix()

page_trend_test_results <- crank::page.trend.test(PerformanceMatrix, ranks = FALSE)

# Inheritances ----
data("Sessions")
data("Guesses")

AncestorMap <- Sessions %>%
  filter_diachronic_exp() %>%
  filter(TeamID != "G47") %>%
  group_by(TeamID) %>%
  arrange(Generation) %>%
  mutate(AncestorID = lag(SessionID)) %>%
  ungroup() %>%
  filter(Generation > 1) %>%
  select(SessionID, AncestorID)

Inheritances <- Guesses %>%
  filter_diachronic_exp() %>%
  filter(TeamID != "G47") %>%
  filter(Generation < 4) %>%
  group_by(SessionID) %>%
  summarize(InheritanceSize = max(SessionInventorySize) - 6) %>%
  rename(AncestorID = SessionID) %>%
  left_join(AncestorMap, .)

exp1$mean_inheritance_size <- round(mean(Inheritances$InheritanceSize), 1)
exp1$sd_inheritance_size <- round(sd(Inheritances$InheritanceSize), 1)

# Learning times ----
data("Guesses")

LearningTimes <- Guesses %>%
  filter_diachronic_exp() %>%
  filter(TeamID != "G47") %>%
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

DiachronicInheritance <- Guesses %>%
  filter_diachronic_exp() %>%
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
  filter_diachronic_exp() %>%
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
  filter_diachronic_exp() %>%
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
