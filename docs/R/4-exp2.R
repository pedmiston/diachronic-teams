# ---- exp2

exp2 <- list()

# Methods ----
data("Teams")
data("Sessions")

TeamCounts50 <- Teams %>%
  filter_exp2() %>%
  count(Strategy, SessionDuration, PlayersPerSession) %>%
  rename(NumTeams = n)

PlayerCounts50 <- Sessions %>%
  filter_exp2() %>%
  left_join(
    Teams %>%
      filter_exp2() %>%
      select(TeamID, SessionDuration, PlayersPerSession) %>%
      unique()
  ) %>%
  select(PlayerID, Strategy, SessionDuration, PlayersPerSession) %>%
  unique() %>%
  count(Strategy, SessionDuration, PlayersPerSession) %>%
  rename(NumParticipants = n)

ConditionCounts50 <- left_join(
    TeamCounts50,
    PlayerCounts50
  ) %>%
  mutate(TeamSize = ifelse(Strategy == "Isolated", 1, 2)) %>%
  select(
    Strategy, SessionDuration, TeamSize, PlayersPerSession, NumTeams, NumParticipants
  ) %>%
  rename(
    `Duration` = SessionDuration,
    `Team Size` = TeamSize,
    `Session Size` = PlayersPerSession,
    `Num Teams` = NumTeams,
    `Num Participants` = NumParticipants
  )

exp2$N <- sum(ConditionCounts50$`Num Participants`)

# Number of innovations ----
data("Guesses")
data("Exp2Manifest")

Exp2Sessions <- filter(Exp2Manifest, TeamStatus == "valid", SessionStatus == "valid") %>%
  select(-(TeamStatus:Notes))

PlayerPerformance50min <- Guesses %>%
  filter_exp2() %>%
  recode_guess_type("UniqueSessionGuess", "UniqueSessionResult") %>%
  group_by(SessionID, GuessType) %>%
  summarize(NumGuesses = n()) %>%
  ungroup() %>%
  left_join(Exp2Sessions) %>%
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

# Guesses per item ----
data("Guesses")
data("AdjacentItems")
data("Teams")

SessionTypes50min <- Sessions%>%
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
