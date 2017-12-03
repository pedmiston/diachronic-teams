source("docs/R/_setup.R")

# ---- exp1

# List to hold descriptives relevant for in-text citation
exp1 <- list()

# Methods ----
TeamCounts50 <- Teams %>%
  filter(TeamStatus == "V", Exp == "50LaborMinutes") %>%
  count(Strategy, SessionDuration, NumPlayers) %>%
  rename(NumTeams = n)

PlayerCounts50 <- Players %>%
  filter(TeamStatus == "V", Exp == "50LaborMinutes") %>%
  left_join(
    Teams %>%
      filter(TeamStatus == "V", Exp == "50LaborMinutes") %>%
      select(TeamID, SessionDuration, NumPlayers, SessionsPerPlayer, PlayersPerSession) %>%
      unique()
  ) %>%
  select(PlayerID, Strategy, SessionDuration, NumPlayers, SessionsPerPlayer, PlayersPerSession) %>%
  unique() %>%
  count(Strategy, SessionDuration, NumPlayers) %>%
  rename(NumParticipants = n)

ConditionCounts50 <- Teams %>%
  filter(TeamStatus == "V", Exp == "50LaborMinutes") %>%
  select(Strategy, SessionDuration, NumPlayers, SessionsPerPlayer, PlayersPerSession) %>%
  unique() %>%
  arrange(Strategy) %>%
  left_join(TeamCounts50) %>%
  left_join(PlayerCounts50) %>%
  select(-SessionsPerPlayer) %>%
  rename(
    `Duration` = SessionDuration,
    `Team Size` = NumPlayers,
    `Session Size` = PlayersPerSession,
    `Num Teams` = NumTeams,
    `Num Participants` = NumParticipants
  )

exp1$N <- sum(ConditionCounts50$`Num Participants`)

# Diachronic G2 ----
data("Guesses")

filter_DG2 <- . %>% dplyr::filter(Exp == "50LaborMinutes", TeamStatus == "V", Strategy == "Diachronic", Generation == 2)

DG2 <- Guesses %>%
  filter_DG2() %>%
  label_stage_time() %>%
  mutate(
    StageTimeMin = StageTime/60,
    StageTimeMin_2 = StageTimeMin * StageTimeMin
  )

DG1Summary <- Guesses %>%
  # filter DG1
  dplyr::filter(Exp == "50LaborMinutes", TeamStatus == "V", Strategy == "Diachronic", Generation == 1) %>%
  group_by(TeamID) %>%
  summarize(InheritanceSize = max(SessionInventorySize)) %>%
  mutate(NumInnovationsInherited = InheritanceSize - 6)

exp1$mean_inheritance_size <- round(mean(DG1Summary$NumInnovationsInherited), 1)
exp1$sd_inheritance_size <- round(sd(DG1Summary$NumInnovationsInherited), 1)

DG2Stages <- DG2 %>%
  group_by(TeamID) %>%
  summarize(EndLearning = max(SessionTime[Stage == "learning"]))

exp1$mean_learning_time_min <- round(mean(DG2Stages$EndLearning)/60, 1)
exp1$proportion_learning_time_min <- round((exp1$mean_learning_time_min/25) * 100, 1)

learning_times_plot <- ggplot(DG2Stages) +
  aes(EndLearning) +
  geom_histogram(binwidth = 5 * 60, center = 2.5 * 60) +
  scale_x_continuous("Time of learning stage", breaks = seq(0, 25 * 60, by = 5 * 60), labels = seq(0, 25, by = 5))

TimeToRecreateInheritance <- left_join(DG1Summary, DG2Stages)

time_to_recreate_inheritance_mod <- lm(
  EndLearning ~ NumInnovationsInherited,
  # Remove outliers who took exorbitantly long to recreate their inheritance
  data = dplyr::filter(TimeToRecreateInheritance, EndLearning < 10 * 60)
)
time_to_recreate_inheritance_preds <- get_lm_mod_preds(time_to_recreate_inheritance_mod) %>%
  rename(EndLearning = fit, SE = se.fit)

learning_times_by_inheritance_size_plot <- ggplot(TimeToRecreateInheritance) +
  aes(NumInnovationsInherited, EndLearning) +
  geom_point() +
  geom_smooth(aes(ymin = EndLearning-SE, ymax = EndLearning + SE),
              stat = "identity", data = time_to_recreate_inheritance_preds) +
  scale_x_continuous("Number of items inherited") +
  scale_y_continuous("Time of learning stage (min)", breaks = seq(0, 25 * 60, by = 5 * 60), labels = seq(0, 25, by = 5))

diachronic_player_stages_plot <- ggplot(DG2) +
  aes(SessionTime, SessionInventorySize) +
  geom_line(aes(group = TeamID), color = t_$color_picker("green")) +
  facet_wrap("TeamID") +
  theme(legend.position = "none")

# This plot is broken because some participants get unique items that their ancestors do not.
diachronic_player_stages_plot_labeled <- diachronic_player_stages_plot +
  geom_hline(aes(yintercept = InheritanceSize),
             data = DG1Summary,
             color = t_$color_picker("orange")) +
  geom_vline(aes(xintercept = EndLearning),
             data = DG2Stages,
             color = t_$color_picker("blue"))

illustrative_teams <- c("G123", "G160", "G164")
illustrative_diachronic_player_stages_plot <- (
    # Replace data in plot with just the data from the illustrative teams
    diachronic_player_stages_plot %+% dplyr::filter(DG2, TeamID %in% illustrative_teams)
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

DG2Playing <- DG2 %>%
  filter(Stage == "playing") %>%
  left_join(DG1Summary)

diachronic_learning_rate_mod <- lmer(
  SessionInventorySize ~ (StageTimeMin + StageTimeMin_2) * InheritanceSize + (StageTimeMin + StageTimeMin_2|TeamID),
  data = DG2Playing
)

mean_inheritance_size <- mean(DG1Summary$InheritanceSize)
sd_inheritance_size <- sd(DG1Summary$InheritanceSize)
sampled_inheritance_sizes <- c(mean_inheritance_size - sd_inheritance_size, mean_inheritance_size, mean_inheritance_size + sd_inheritance_size)

diachronic_learning_rate_preds <- expand.grid(
    StageTimeMin = seq(0, 20, by = 1),
    InheritanceSize = sampled_inheritance_sizes
  ) %>%
  mutate(StageTimeMin_2 = StageTimeMin * StageTimeMin) %>%
  cbind(., predictSE(diachronic_learning_rate_mod, newdata = ., se = TRUE)) %>%
  rename(SessionInventorySize = fit, SE = se.fit)

diachronic_player_trajectories_plot <- ggplot(DG2) +
  aes(StageTimeMin, SessionInventorySize) +
  geom_line(aes(group = TeamID), color = t_$color_picker("green")) +
  geom_vline(xintercept = 0, color = t_$color_picker("blue")) +
  geom_smooth(aes(ymin = SessionInventorySize - SE, ymax = SessionInventorySize + SE, group = InheritanceSize),
              stat = "identity", data = diachronic_learning_rate_preds,
              color = t_$color_picker("orange")) +
  scale_x_continuous("Playin time (min)",
                     breaks = seq(-25, 25, by = 5),
                     labels = seq(-25, 25, by = 5)) +
  scale_y_continuous("Inventory Size") +
  t_$base_theme +
  ggtitle("All participants")
diachronic_player_trajectories_plot

# Number of innovations ----
data("PlayerPerformance")

PlayerPerformance50min <- PlayerPerformance %>%
  filter(TeamStatus == "V", Exp == "50LaborMinutes") %>%
  recode_strategy() %>%
  recode_session_type_50min() %>%
  label_inheritance()

num_innovations_50min_mod <- lmer(
  NumInnovations ~ DG2_v_DG1 + DG2_v_I50 + DG2_v_S2 + (1|TeamID),
  data = PlayerPerformance50min
)

num_innovations_50min_teamwork_mod <- lmer(
  NumInnovations ~ DSvI + DvS + (1|TeamID),
  data = filter(PlayerPerformance50min, SessionType != "DG1")
)


# Use lm mod for error on plot because lmer mod preds with AICcmodavg look too small
num_innovations_50min_lm_mod <- lm(
  NumInnovations ~ DG2_v_DG1 + DG2_v_I50 + DG2_v_S2,
  data = PlayerPerformance50min
)

num_innovations_50min_preds <- recode_session_type_50min() %>%
  cbind(., predict(num_innovations_50min_lm_mod, newdata = ., se = TRUE)) %>%
  rename(NumInnovations = fit, SE = se.fit) %>%
  recode_strategy() %>%
  recode_session_type_50min() %>%
  label_inheritance()

set.seed(432)
num_innovations_50min_plot <- ggplot(PlayerPerformance50min) +
  aes(SessionTypeOrdered, NumInnovations) +
  geom_bar(aes(fill = StrategyLabel, alpha = Inheritance),
           stat = "identity", data = num_innovations_50min_preds) +
  geom_point(aes(color = StrategyLabel),
             position = position_jitter(width = 0.3)) +
  geom_linerange(aes(ymin = NumInnovations-SE, ymax = NumInnovations+SE),
                 data = num_innovations_50min_preds) +
  t_$scale_color_strategy +
  t_$scale_fill_strategy +
  scale_alpha_manual(values = c(0.7, 0.4)) +
  xlab("") +
  t_$scale_y_num_innovations +
  scale_shape_manual(values = c(16, 1)) +
  t_$base_theme +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  ) +
  ggtitle("Number of innovations discovered using each strategy")

# Rate of innovation ----
data("Sampled")

Sampled50min <- Sampled %>%
  filter(
    TeamStatus == "V",
    Exp == "50LaborMinutes"
  ) %>%
  recode_strategy() %>%
  label_inheritance() %>%
  mutate(NumInnovations = InventorySize - 6)

innovation_rate_50min_plot <- ggplot(Sampled50min) +
  aes(TeamTime, NumInnovations) +
  geom_line(aes(color = StrategyLabel,
                group = interaction(StrategyLabel, Generation),
                size = Inheritance),
            stat = "summary", fun.y = "mean") +
  t_$scale_x_team_time +
  t_$scale_color_strategy +
  scale_size_manual(values = c(1.8, 1.0)) +
  t_$scale_y_num_innovations +
  guides(linetype = "none", size = "none") +
  t_$base_theme +
  theme(legend.position = c(0.2, 0.8))

# Types of time ----
diachronic <- data_frame(
  Strategy = "Diachronic",
  CalendarHours = 1:100,
  LaborHours = CalendarHours,
  Person = rep(c(1, 2), each = 50)
)

isolated <- diachronic %>%
  mutate(
    Strategy = "Isolated",
    Person = 1
  )

synchronic <- data_frame(
  Strategy = "Synchronic",
  CalendarHours = 1:50,
  LaborHours = CalendarHours * 2,
  Person = 1
)

time <- rbind(diachronic, synchronic, isolated) %>%
  group_by(Strategy, Person) %>%
  mutate(PersonHours = 1:n()) %>%
  ungroup() %>%
  recode_strategy() %>%
  mutate(
    # Separate Diachronic and Isolated lines
    LaborHours = ifelse(Strategy == "Synchronic", LaborHours,
                        ifelse(Strategy == "Diachronic", LaborHours + 0.8, LaborHours - 0.8))
  )

axis_breaks <- c(0, 50, 100)
axis_labels <- c(0, expression(1/2), 1)

gg_time <- ggplot(time, aes(CalendarHours, LaborHours)) +
  geom_line(aes(color = StrategyLabel), size = 1.2) +
  scale_x_continuous("Calendar time", breaks = axis_breaks, labels = axis_labels) +
  scale_y_continuous("Labor time", breaks = axis_breaks, labels = axis_labels) +
  t_$scale_color_strategy +
  scale_linetype_manual(values = c(1, 1, 2)) +
  guides(color = guide_legend("", reverse = TRUE), linetype = "none") +
  t_$base_theme +
  theme(legend.position = c(0.75, 0.2))

time %<>%
  mutate(StrategyIsolated = factor(Strategy, levels = c("Synchronic", "Diachronic", "Isolated")))

gg_person <- ggplot(time) +
  aes(StrategyIsolated, PersonHours) +
  geom_bar(aes(fill = StrategyLabel), stat = "summary", fun.y = "max",
           alpha = 0.8) +
  scale_x_discrete("") +
  scale_y_continuous("Learning time", breaks = axis_breaks, labels = axis_labels) +
  t_$scale_fill_strategy +
  t_$base_theme +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank())

# Guesses per item ----
data("Guesses")
data("AdjacentItems")
data("Teams")

SessionTypes50min <- Players %>%
  filter(Exp == "50LaborMinutes", TeamStatus == "V") %>%
  recode_session_type_50min() %>%

  # Collapse Synchronic players into a single team,
  # but leave Diachronic and Isolated players alone.
  select(Strategy, SessionType, TeamID, Generation) %>%
  unique()

GuessesPerItem50min <- Guesses %>%
  filter(Exp == "50LaborMinutes", TeamStatus == "V") %>%

  # Treat all Synchronic players as "playing" even if they are lagging.
  mutate(Stage = ifelse(Strategy == "Synchronic", "playing", Stage)) %>%

  # Copy guesses for each adjacent item
  left_join(AdjacentItems, by = c("PrevSessionInventoryID" = "ID")) %>%
  recode_session_type_50min()

CostPerItem50min <- GuessesPerItem50min %>%
  # Summarize the cost of each item for each session type: DG1, DG2, I50, S2.
  # Cost scores are summed across both synchronic players.
  group_by(TeamID, Generation, Adjacent, SessionType) %>%
  summarize(
    TotalGuesses = n(),
    TotalTime = sum(GuessTime, na.rm = TRUE),
    Discovered = any(Result == Adjacent)
  ) %>%
  ungroup() %>%

  # Re-label summarized data
  left_join(SessionTypes50min) %>%
  recode_strategy() %>%
  recode_session_type_50min() %>%
  recode_discovered() %>%
  label_inheritance() %>%
  recode_inheritance()

# Guesses per item by inheritance.
# Determine the impact of inheritance on guessing ability
# by comparing guessing rates for each item discovered
# by Diachronic players.
guesses_per_item_by_inheritance_mod <- lmer(
  TotalGuesses ~ Diachronic_v_NoInheritance +
    (Diachronic_v_NoInheritance|Adjacent),
  data = filter(CostPerItem50min, Discovered))

guesses_per_item_by_inheritance_preds <- recode_inheritance() %>%
  filter(Inheritance != "individual_inheritance") %>%
  cbind(., predictSE(guesses_per_item_by_inheritance_mod, newdata = ., se = TRUE)) %>%
  rename(TotalGuesses = fit, SE = se.fit)

guesses_per_item_treatment_mod <- lmer(
  TotalGuesses ~ DG2_v_DG1 + DG2_v_S2 + DG2_v_I50 +
    (DG2_v_DG1 + DG2_v_S2 + DG2_v_I50|Adjacent),
  data = filter(CostPerItem50min, Discovered))
guesses_per_item_treatment_preds <- recode_session_type_50min() %>%
  cbind(., predictSE(guesses_per_item_treatment_mod, newdata = ., se = TRUE)) %>%
  rename(TotalGuesses = fit, SE = se.fit)

guesses_per_item_by_inheritance_plot <- ggplot(CostPerItem50min) +
  aes(InheritanceLabel, TotalGuesses) +
  geom_line(aes(group = Adjacent), color = t_$color_picker("blue"),
            stat = "summary", fun.y = "mean",
            size = 0.8) +
  geom_line(aes(group = 1),
            stat = "identity", data = guesses_per_item_by_inheritance_preds,
            size = 1.2) +
  geom_errorbar(aes(ymin = TotalGuesses-SE, ymax = TotalGuesses+SE),
                data = guesses_per_item_by_inheritance_preds,
                width = 0.05, size = 1.2) +
  xlab("") +
  scale_y_continuous("Guesses per item") +
  t_$base_theme



# Guesses per item: Playing ----
CostPerItem50minPlaying <- GuessesPerItem50min %>%
  filter(Stage == "playing") %>%
  # Summarize the cost of each item for each session type: DG1, DG2, I50, S2.
  # Cost scores are summed across both synchronic players.
  group_by(TeamID, Generation, Adjacent, SessionType) %>%
  summarize(
    TotalGuesses = n(),
    TotalTime = sum(GuessTime, na.rm = TRUE),
    Discovered = any(Result == Adjacent)
  ) %>%
  ungroup() %>%

  # Re-label summarized data
  left_join(SessionTypes50min) %>%
  recode_strategy() %>%
  recode_session_type_50min() %>%
  recode_discovered() %>%
  label_inheritance() %>%
  recode_inheritance()

# Guesses per item by inheritance.
# Determine the impact of inheritance on guessing ability
# by comparing guessing rates for each item discovered
# by Diachronic players.
guesses_per_new_item_by_inheritance_mod <- lmer(
  TotalGuesses ~ Diachronic_v_NoInheritance +
    (Diachronic_v_NoInheritance|Adjacent),
  data = filter(CostPerItem50minPlaying, Discovered))

guesses_per_new_item_by_inheritance_preds <- recode_inheritance() %>%
  filter(Inheritance != "individual_inheritance") %>%
  cbind(., predictSE(guesses_per_new_item_by_inheritance_mod, newdata = ., se = TRUE)) %>%
  rename(TotalGuesses = fit, SE = se.fit)

guesses_per_new_item_by_inheritance_plot <- ggplot(CostPerItem50minPlaying) +
  aes(InheritanceLabel, TotalGuesses) +
  geom_line(aes(group = Adjacent), color = t_$color_picker("blue"),
            stat = "summary", fun.y = "mean",
            size = 0.8) +
  geom_line(aes(group = 1),
            stat = "identity", data = guesses_per_new_item_by_inheritance_preds,
            size = 1.2) +
  geom_errorbar(aes(ymin = TotalGuesses-SE, ymax = TotalGuesses+SE),
                data = guesses_per_new_item_by_inheritance_preds,
                width = 0.05, size = 1.2) +
  xlab("") +
  scale_y_continuous("Guesses per item") +
  t_$base_theme

# Guess types ----
data("PlayerPerformance")

GuessTypes50min <- PlayerPerformance %>%
  filter(TeamStatus == "V", Exp == "50LaborMinutes") %>%
  recode_strategy() %>%
  recode_session_type_50min() %>%
  mutate(
    PropRedundantGuesses = NumRedundantGuesses/NumGuesses,
    PropRepeatedItems = NumRepeatedItems/NumGuesses,
    PropUniqueGuesses = NumUniqueGuesses/NumGuesses,
    PropUniqueItems = NumInnovations/NumGuesses
  )

prop_redundant_guesses_mod <- lm(PropRedundantGuesses ~ DG2_v_DG1 + DG2_v_I50 + DG2_v_S2,
                                 data = GuessTypes50min)

prop_unique_guesses_mod <- lm(PropUniqueGuesses ~ DG2_v_DG1 + DG2_v_I50 + DG2_v_S2,
                              data = GuessTypes50min)


GuessTypes50minSummary <- PlayerPerformance %>%
  filter(TeamStatus == "V", Exp == "50LaborMinutes") %>%
  recode_strategy() %>%
  recode_session_type_50min() %>%
  group_by(SessionTypeSimple) %>%
  summarize(
    NumGuesses = sum(NumGuesses),
    NumRedundantGuesses = sum(NumRedundantGuesses),
    NumRepeatedItems = sum(NumRepeatedItems),
    NumUniqueGuesses = sum(NumUniqueGuesses),
    NumInnovations = sum(NumInnovations)
  ) %>%
  ungroup() %>%
  mutate(
    PropRedundantGuesses = NumRedundantGuesses/NumGuesses,
    PropRepeatedItems = NumRepeatedItems/NumGuesses,
    PropUniqueGuesses = NumUniqueGuesses/NumGuesses,
    PropUniqueItems = NumInnovations/NumGuesses
  ) %>%
  select(SessionTypeSimple, PropRedundantGuesses, PropRepeatedItems, PropUniqueGuesses, PropUniqueItems) %>%
  gather(PropGuessType, PropGuesses, -SessionTypeSimple) %>%
  recode_session_type_50min() %>%
  recode_prop_guess_type_total()

prop_guess_types_50min_plot <- ggplot(GuessTypes50minSummary) +
  aes(SessionTypeOrdered, PropGuesses, fill = PropGuessTypeLabel) +
  geom_bar(stat = "identity") +
  xlab("") +
  scale_y_continuous("Proportion of guesses", labels = scales::percent) +
  scale_fill_manual("Guess types",
                    values = t_$color_picker(c("green", "blue", "orange", "pink"))) +
  t_$base_theme
