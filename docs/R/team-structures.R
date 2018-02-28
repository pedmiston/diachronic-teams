# ---- setup
library(magrittr)
library(broom)
library(lme4)
library(AICcmodavg)
library(gridExtra)
library(crotchet)
library(totems)
library(tidyverse) # Load tidyverse after totems to prevent dplyr::filter from being masked
t_ <- load_totems_theme()

# ---- intro
# Intro ----

# * Types of time ----
# Makes "types of time" plot.
# Types of time plot shows the relationship
# between labor time, calendar time, and learning time.

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
    LaborHours = ifelse(Strategy == "Synchronic", LaborHours,
                        ifelse(Strategy == "Diachronic", LaborHours + 1, LaborHours - 1))
  )

axis_breaks <- c(0, 50, 100)
axis_labels <- c(0, expression(1/2), 1)

labels <- recode_strategy() %>%
  mutate(
    CalendarHours = 25,
    LaborHours = c(60, 18, 35),
    Angle = c(67.5, 47, 47)
  )

gg_time <- ggplot(time, aes(CalendarHours, LaborHours)) +
  geom_line(aes(color = StrategyLabel), size = 1.2) +
  geom_text(aes(label = StrategyLabel, angle = Angle), data = labels, hjust = 0, size = 2.5) +
  scale_x_continuous("Calendar time", breaks = axis_breaks, labels = axis_labels) +
  scale_y_continuous("Labor time", breaks = axis_breaks, labels = axis_labels) +
  t_$scale_color_strategy +
  scale_linetype_manual(values = c(1, 1, 2)) +
  guides(color = "none", linetype = "none") +
  t_$base_theme

time %<>%
  mutate(StrategyIsolated = factor(Strategy, levels = c("Synchronic", "Diachronic", "Isolated")))

labels <- recode_strategy() %>%
  mutate(
    StrategyIsolated = factor(Strategy, levels = c("Synchronic", "Diachronic", "Isolated")),
    PersonHours = 5
  )

gg_person <- ggplot(time) +
  aes(StrategyIsolated, PersonHours) +
  geom_bar(aes(fill = StrategyLabel), stat = "summary", fun.y = "max",
           alpha = 0.8) +
  geom_text(aes(label = StrategyLabel), angle = 90, data = labels, hjust = 0, size = 2.5) +
  scale_x_discrete("", labels = NULL) +
  scale_y_continuous("Learning time", breaks = axis_breaks, labels = axis_labels) +
  t_$scale_fill_strategy +
  t_$base_theme +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank())

types_of_time_plot <- gridExtra::arrangeGrob(
  crotchet::read_graphviz("team-structures", package = "totems"),
  gg_time,
  gg_person,
  nrow = 1,
  widths = c(0.25, 0.375, 0.375)
)

# ---- methods
# Methods ----

data("Teams")
data("Sessions")

methods <- list()  # Store vars for in-text reference
methods$n_unique_guesses_6 <- count_unique_guesses(6)
methods$n_unique_guesses_6_pct <- round(3/methods$n_unique_guesses_6 * 100, 1)

report_lmer_mod <- function(lmer_mod, term, formats = NULL, reverse_sign = FALSE) {
  term_ <- term  # work around NSE in filter
  results <- broom::tidy(lmer_mod, effects = "fixed") %>%
    filter(term == term_) %>%
    as.list()

  if(reverse_sign) {
    results$estimate <- -results$estimate
    results$statistic <- -results$statistic
  }

  fmt <- c(b=2, se=2, t=2)
  if(!is.null(formats)) fmt[names(formats)] <- formats

  fstring <- sprintf("_b_ = %%.%sf (SE = %%.%sf), _t_ = %%.%sf", fmt["b"], fmt["se"], fmt["t"])
  sprintf(fstring, results$estimate, results$std.error, results$statistic)
}

report_lm_mod <- function(lm_mod, term, min_p_value = 0.001, p_value_only = FALSE) {
  term_ <- term  # work around NSE in call to filter
  results <- broom::tidy(lm_mod) %>%
    filter(term == term_) %>%
    as.list()

  lm_summary <- broom::glance(lm_mod) %>% as.list()
  results$df <- lm_summary$df.residual

  results$p_value_str <- compute_p_string(results$p.value)

  if (p_value_only == TRUE) {
    return(results$p_value_str)
  }

  sprintf("_b_ = %.2f (SE = %.2f), _t_(%.1f) = %.2f, %s",
          results$estimate, results$std.error, results$df, results$statistic, results$p_value_str)
}

report_glm_mod <- function(glm_mod, term, min_p_value = 0.001, p_value_only = FALSE) {
  term_ <- term  # work around NSE in call to filter
  results <- broom::tidy(glm_mod) %>%
    filter(term == term_) %>%
    as.list()

  glm_summary <- broom::glance(glm_mod) %>% as.list()
  results$df <- glm_summary$df.residual

  results$p_value_str <- compute_p_string(results$p.value)

  sprintf("_b_ = %.2f logodds (SE = %.2f), _z_ = %.2f, %s",
          results$estimate, results$std.error, results$statistic, results$p_value_str)
}

report_beta <- function(mod, param, digits = 1, transform = NULL) {
  param_ <- param # prevent masking in NSE
  estimate <- mod %>%
    summary %>%
    .$coefficients %>%
    as.data.frame() %>%
    rownames_to_column("param") %>%
    filter(param == param_) %>%
    .$Estimate
  if(!is.null(transform)) estimate <- transform(estimate)
  round(estimate, digits = digits)
}

report_modcomp <- function(modcomp) {
  modcomp <- as.list(modcomp[2, ])
  p_string <- compute_p_string(modcomp$`Pr(>Chisq)`)
  print(sprintf("$\\chi^2$(%i) = %.4f, %s", modcomp$`Chi Df`, modcomp$Chisq, p_string))
}

report_page_test <- function(page_trend_test_results) {
  page_trend_test_results$p_val_str <- compute_p_string(page_trend_test_results$px2)
  print(sprintf("Page's _L_ = %.0f, $\\chi^2$ = %.0f, %s",
                page_trend_test_results$L,
                page_trend_test_results$x2L,
                page_trend_test_results$p_val_str))
}

compute_p_string <- function(p_value) {
  min_p_value <- 0.001
  if (p_value < min_p_value) {
    p_value_str <- "_p_ < 0.001"
  } else {
    p_value_str <- paste("_p_ = ", round(p_value, 3))
  }
  p_value_str
}

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


TeamCounts <- Teams %>%
  filter(
    SessionDuration %in% c(25, 50),
    !(Strategy == "Synchronic" & !(PlayersPerSession %in% c(2, 4)))
  ) %>%
  count(Strategy, SessionDuration, PlayersPerSession) %>%
  rename(NumTeams = n)

PlayerCounts <- Sessions %>%
  left_join(
    Teams %>%
      select(TeamID, SessionDuration, PlayersPerSession) %>%
      unique()
  ) %>%
  filter(
    SessionDuration %in% c(25, 50),
    !(Strategy == "Synchronic" & !(PlayersPerSession %in% c(2, 4)))
  ) %>%
  select(PlayerID, Strategy, SessionDuration, PlayersPerSession) %>%
  unique() %>%
  count(Strategy, SessionDuration, PlayersPerSession) %>%
  rename(NumPlayers = n)

ConditionCounts <- Teams %>%
  filter(
    SessionDuration %in% c(25, 50),
    !(Strategy == "Synchronic" & !(PlayersPerSession %in% c(2, 4)))
  ) %>%
  select(Strategy, SessionDuration, PlayersPerSession) %>%
  unique() %>%
  arrange(Strategy) %>%
  left_join(TeamCounts) %>%
  left_join(PlayerCounts)

# ---- exp1
# 50min ----

# List to hold descriptives for in-text citation
exp1 <- list()

# * Methods ----
data("Sessions")

Exp2Participants <- Sessions %>%
  filter_50min() %>%
  count(Strategy) %>%
  rename(`$N_{participants}$` = n)

Exp1Teams <- Sessions %>%
  filter_50min() %>%
  select(Strategy, TeamID) %>%
  unique() %>%
  count(Strategy) %>%
  rename(`$N_{teams}$` = n)

Exp1N <- left_join(Exp2Participants, Exp1Teams)

exp1$n_participants <- sum(Exp1N$`$N_{participants}$`)

# * Number of innovations ----
data("Guesses")

Innovations <- Guesses %>%
  filter_50min() %>%
  recode_guess_type("UniqueSessionGuess", "UniqueSessionResult") %>%
  group_by(SessionID) %>%
  summarize(NumInnovations = sum(GuessType == "unique_item")) %>%
  ungroup() %>%
  left_join(Sessions) %>%
  recode_strategy() %>%
  recode_session_type_50min() %>%
  label_inheritance()

num_innovations_50min_mod <- lmer(
  NumInnovations ~ DG2_v_DG1 + DG2_v_I50 + DG2_v_S2 + (1|TeamID),
  data = Innovations
)

exp1$DG2_v_DG1 <- report_lmer_mod(num_innovations_50min_mod, "DG2_v_DG1", reverse_sign = TRUE)
exp1$DG2_v_I50 <- report_lmer_mod(num_innovations_50min_mod, "DG2_v_I50", reverse_sign = TRUE)
exp1$S2_v_DG2 <- report_lmer_mod(num_innovations_50min_mod, "DG2_v_S2", reverse_sign = FALSE)

num_innovations_50min_teamwork_mod <- lmer(
  NumInnovations ~ DSvI + DvS + (1|TeamID),
  data = filter(Innovations, SessionType != "DG1")
)

exp1$teamwork_stats <- report_lmer_mod(num_innovations_50min_teamwork_mod, "DSvI", reverse_sign = TRUE)
exp1$teamwork_residual <- report_lmer_mod(num_innovations_50min_teamwork_mod, "DvS")

# Use lm mod for error on plot because lmer mod preds with AICcmodavg look too small
num_innovations_50min_lm_mod <- lm(
  NumInnovations ~ DG2_v_DG1 + DG2_v_I50 + DG2_v_S2,
  data = Innovations
)

num_innovations_50min_preds <- recode_session_type_50min() %>%
  cbind(., predict(num_innovations_50min_lm_mod, newdata = ., se = TRUE)) %>%
  rename(NumInnovations = fit, SE = se.fit) %>%
  recode_strategy() %>%
  recode_session_type_50min() %>%
  label_inheritance()

set.seed(432)
num_innovations_50min_plot <- ggplot(Innovations) +
  aes(SessionTypeSimple, NumInnovations) +
  geom_bar(aes(fill = StrategyLabel, alpha = Inheritance),
           stat = "identity", data = num_innovations_50min_preds) +
  geom_point(aes(color = StrategyLabel),
             position = position_jitter(width = 0.3), shape = 1) +
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
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

# * Rate of innovation ----
data("Sampled")

Sampled50min <- Sampled %>%
  filter_50min() %>%
  recode_strategy() %>%
  label_inheritance() %>%
  mutate(NumInnovations = InventorySize - 6)

labels <- recode_session_type_50min() %>%
  mutate(TeamTime = c(12.5, 43.75, 43.75, 43.75),
         NumInnovations = c(4.5, 8.7, 6.8, 10)) %>%
  recode_strategy()

innovation_rate_50min_plot <- ggplot(Sampled50min) +
  aes(TeamTime, NumInnovations) +
  geom_line(aes(color = StrategyLabel,
                group = interaction(StrategyLabel, Generation),
                size = Inheritance),
            stat = "summary", fun.y = "mean") +
  geom_text(aes(label = SessionTypeSimple, color = StrategyLabel), data = labels, fontface = "bold", size = 2.5) +
  t_$scale_x_team_time +
  t_$scale_color_strategy +
  scale_size_manual(values = c(1.8, 1.0)) +
  t_$scale_y_num_innovations +
  guides(linetype = "none", size = "none") +
  t_$base_theme +
  theme(legend.position = "none")

# * Guesses per item ----
data("Guesses")
data("AdjacentItems")
data("Teams")

SessionTypes50min <- Sessions %>%
  filter_50min() %>%
  recode_session_type_50min() %>%

  # Collapse Synchronic players into a single team,
  # but leave Diachronic and Isolated players alone.
  select(Strategy, SessionType, TeamID, Generation) %>%
  unique()

GuessesPerItem50min <- Guesses %>%
  filter_50min() %>%

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
guesses_per_item_treatment_mod <- lmer(
  TotalGuesses ~ DG2_v_DG1 + DG2_v_S2 + DG2_v_I50 +
    (DG2_v_DG1 + DG2_v_S2 + DG2_v_I50|Adjacent),
  data = filter(CostPerItem50min, Discovered))

guesses_per_item_treatment_preds <- recode_session_type_50min() %>%
  cbind(., predictSE(guesses_per_item_treatment_mod, newdata = ., se = TRUE)) %>%
  rename(TotalGuesses = fit, SE = se.fit) %>%
  recode_strategy()

guesses_per_item_treatment_plot <- ggplot(CostPerItem50min) +
  aes(SessionTypeSimple, TotalGuesses) +
  geom_point(aes(group = Adjacent, color = StrategyLabel), position = position_jitter(width = 0.2),
             stat = "summary", fun.y = "mean", shape = 1) +
  geom_errorbar(aes(ymin = TotalGuesses-SE, ymax = TotalGuesses+SE, color = StrategyLabel),
                data = guesses_per_item_treatment_preds,
                width = 0.1, size = 1.2) +
  xlab("") +
  scale_y_continuous("Guesses per innovation", breaks = seq(0, 300, by = 50)) +
  t_$scale_color_strategy +
  t_$base_theme +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )

# * Guesses per item: Playing ----
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

# * Guess types ----
GuessTypes <- Guesses %>%
  filter_50min() %>%
  recode_guess_type("UniqueSessionGuess", "UniqueSessionResult") %>%
  group_by(SessionID) %>%
  summarize(
    NumGuesses = n(),
    NumRedundantGuesses = sum(GuessType == "redundant"),
    NumRepeatedItems = sum(GuessType == "repeat_item"),
    NumUniqueGuesses = sum(GuessType == "unique_guess"),
    NumUniqueItems = sum(GuessType == "unique_item")
  ) %>%
  ungroup() %>%
  left_join(Sessions) %>%
  recode_strategy() %>%
  recode_session_type_50min() %>%
  mutate(
    PropRedundantGuesses = NumRedundantGuesses/NumGuesses,
    PropRepeatedItems = NumRepeatedItems/NumGuesses,
    PropUniqueGuesses = NumUniqueGuesses/NumGuesses,
    PropUniqueItems = NumUniqueItems/NumGuesses
  )

prop_redundant_guesses_mod <- lm(PropRedundantGuesses ~ DG2_v_DG1 + DG2_v_I50 + DG2_v_S2,
                                 data = GuessTypes)

prop_unique_guesses_mod <- lm(PropUniqueGuesses ~ DG2_v_DG1 + DG2_v_I50 + DG2_v_S2,
                              data = GuessTypes)

GuessTypes50minSummary <- Guesses %>%
  filter_50min() %>%
  recode_guess_type("UniqueSessionGuess", "UniqueSessionResult") %>%
  recode_session_type_50min() %>%
  group_by(SessionTypeSimple) %>%
  summarize(
    NumGuesses = n(),
    NumRedundantGuesses = sum(GuessType == "redundant"),
    NumRepeatedItems = sum(GuessType == "repeat_item"),
    NumUniqueGuesses = sum(GuessType == "unique_guess"),
    NumUniqueItems = sum(GuessType == "unique_item")
  ) %>%
  ungroup() %>%
  mutate(
    PropRedundantGuesses = NumRedundantGuesses/NumGuesses,
    PropRepeatedItems = NumRepeatedItems/NumGuesses,
    PropUniqueGuesses = NumUniqueGuesses/NumGuesses,
    PropUniqueItems = NumUniqueItems/NumGuesses
  ) %>%
  recode_session_type_50min() %>%
  select(SessionTypeSimple, PropRedundantGuesses, PropRepeatedItems, PropUniqueGuesses, PropUniqueItems) %>%
  gather(PropGuessType, PropGuesses, -SessionTypeSimple) %>%
  recode_session_type_50min() %>%
  recode_prop_guess_type_total()

prop_guess_types_50min_plot <- ggplot(GuessTypes50minSummary) +
  aes(SessionTypeSimple, PropGuesses, fill = PropGuessTypeLabel) +
  geom_bar(stat = "identity") +
  xlab("") +
  scale_y_continuous("Proportion of guesses", labels = scales::percent) +
  scale_fill_manual("Guess types",
                    values = t_$color_picker(c("green", "blue", "orange", "pink"))) +
  t_$base_theme +
  theme(panel.grid.major.x = element_blank())

# ---- selfother
# SelfOther ----
# * Methods ----
data("Sessions")

Exp3Participants <- Sessions %>%
  filter_exp3() %>%
  select(Strategy, PlayerID) %>%
  unique() %>%
  count(Strategy) %>%
  rename(N = n)

# * Innovations by generation ----
data("Guesses")
data("Sessions")

Innovations <- Guesses %>%
  filter_exp3() %>%
  recode_guess_type(unique_guess = "UniqueSessionGuess", unique_result = "UniqueSessionResult") %>%
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
  geom_line(aes(GenerationJittered, group = TeamID, color = Strategy),
            alpha = 0.6) +
  # geom_line(aes(group = 1), data = innovations_by_generation_preds,
  #           color = t_$color_picker("blue"), size = 1.5) +
  # geom_errorbar(aes(ymin = NumInnovations-SE, ymax = NumInnovations+SE),
  #               data = innovations_by_generation_preds,
  #               color = t_$color_picker("blue"), width = 0.2, size = 1.5) +
  geom_point(stat = "summary", fun.y = "mean",
             color = t_$color_picker("green"), shape = 4, size = 3) +
  facet_wrap("Strategy") +
  # t_$scale_y_num_innovations +
  t_$base_theme +
  theme(
    panel.grid.minor.x = element_blank()
  )

# * Cost by stage ----
data("Guesses")
data("Players")
data("AdjacentItems")

IndividualPlayers <- filter(Players, Strategy != "Synchronic", Exp == "100LaborMinutes", TeamStatus == "V")

IndividualGuesses <- Guesses %>%
  filter(Strategy != "Synchronic", Exp == "100LaborMinutes", TeamStatus == "V") %>%
  left_join(AdjacentItems, by = c("PrevSessionInventoryID" = "ID")) %>%
  label_stage_ix()

IndividualStages <- IndividualGuesses %>%
  group_by(SessionID, Stage, Result) %>%
  summarize(
    NumGuesses = n()
  ) %>%
  left_join(IndividualPlayers) %>%
  highlight_inheritance_100() %>%
  # Should not be necessary!
  filter(!is.na(Inheritance))

individual_stages_mod <- lmer(
  NumGuesses ~ Diachronic_v_Individual * Stage +
    (Stage|TeamID),
  data = IndividualStages
)

individual_stages_preds <- expand.grid(
  Stage = c("learning", "playing"),
  Inheritance = c("diachronic_inheritance", "individual_inheritance"),
  stringsAsFactors = FALSE
) %>%
  recode_inheritance() %>%
  cbind(., predictSE(individual_stages_mod, newdata = ., se = TRUE)) %>%
  rename(NumGuesses = fit, SE = se.fit)

individual_stages_plot <- ggplot(IndividualStages) +
  aes(Stage, NumGuesses, color = Inheritance) +
  geom_line(aes(group = Inheritance),
            stat = "identity", data = individual_stages_preds) +
  geom_errorbar(aes(ymin = NumGuesses-SE, ymax = NumGuesses+SE),
                data = individual_stages_preds,
                width = 0.2)

FirstDiscovery <- IndividualGuesses %>%
  label_inheritance() %>%
  filter(StageIX == 0, Inheritance != "no_inheritance") %>%
  group_by(SessionID, Result) %>%
  summarize(
    NumGuesses = n()
  ) %>%
  ungroup() %>%
  left_join(IndividualPlayers) %>%
  highlight_inheritance_100() %>%
  # Should not be necessary!
  filter(!is.na(Inheritance)) %>%
  recode_inheritance() %>%
  recode_strategy()

first_discovery_mod <- lm(NumGuesses ~ Diachronic_v_Individual,
                          data = FirstDiscovery)

first_discovery_preds <- recode_inheritance() %>%
  filter(Inheritance != "no_inheritance") %>%
  cbind(., predict(first_discovery_mod, newdata = ., se = TRUE)) %>%
  rename(NumGuesses = fit, SE = se.fit)

first_discovery_plot <- ggplot(FirstDiscovery) +
  aes(StrategyLabel, NumGuesses) +
  # geom_bar(aes(fill = StrategyLabel),
  #          stat = "identity", data = first_discovery_preds,
  #          alpha = 0.6) +
  # geom_linerange(aes(ymin = NumGuesses - SE, ymax = NumGuesses + SE),
  #                data = first_discovery_preds) +
  geom_bar(aes(fill = StrategyLabel),
           stat = "summary", fun.y = "mean",
           alpha = 0.6) +
  geom_point(aes(color = StrategyLabel),
             position = position_jitter(width = 0.2)) +
  scale_y_continuous("Number of guesses") +
  t_$scale_color_strategy +
  t_$scale_fill_strategy +
  t_$base_theme +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank()) +
  ggtitle("Cost of first innovation")

first_discovery_by_generation_plot <- ggplot(FirstDiscovery) +
  aes(Generation, NumGuesses) +
  geom_bar(aes(fill = StrategyLabel, group = factor(Generation)),
           stat = "summary", fun.y = "mean",
           alpha = 0.6, width = 0.8) +
  geom_point(aes(color = StrategyLabel),
             position = position_jitter(width = 0.2)) +
  facet_wrap("StrategyLabel") +
  scale_x_continuous(breaks = 2:4) +
  t_$scale_fill_strategy +
  t_$scale_color_strategy +
  t_$base_theme +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  ggtitle("Cost of first innovation")

# ---- teamsize
# TeamSize ----
# * Methods ----

# * Number of innovations ----
data("Guesses")

drop_isolated <- . %>% filter(Strategy != "Isolated")

Innovations50min <- Guesses %>%
  filter_50min() %>%
  drop_isolated() %>%
  recode_guess_type("UniqueSessionGuess", "UniqueSessionResult") %>%
  group_by(SessionID) %>%
  summarize(NumInnovations = sum(GuessType == "unique_item")) %>%
  ungroup() %>%
  left_join(Sessions) %>%
  recode_strategy() %>%
  mutate(NumPlayers = 2)

Innovations100min <- Guesses %>%
  filter_teamsize() %>%
  drop_isolated() %>%
  recode_guess_type("UniqueSessionGuess", "UniqueSessionResult") %>%
  group_by(SessionID) %>%
  summarize(NumInnovations = sum(GuessType == "unique_item")) %>%
  ungroup() %>%
  left_join(Sessions) %>%
  recode_strategy() %>%
  mutate(NumPlayers = 4)

recode_team_size <- function(frame) {
  team_size_labels <- c("Two person teams", "Four person teams")
  team_size_map <- data_frame(
    NumPlayers = c(2, 4),
    NumPlayersLabel = factor(team_size_labels, levels = team_size_labels)
  )
  if(missing(frame)) return(team_size_map)
  left_join(frame, team_size_map)
}

Innovations <- bind_rows(Innovations50min, Innovations100min) %>%
  group_by(Strategy, NumPlayers, TeamID) %>%
  summarize(NumInnovations = max(NumInnovations)) %>%
  ungroup() %>%
  recode_strategy() %>%
  recode_team_size()

max_innovations_by_teamsize_mod <- lmer(
  NumInnovations ~ Diachronic_v_Synchronic * NumPlayers + (1|TeamID),
  data = Innovations)
max_innovations_by_teamsize_preds <- expand.grid(
  Strategy = c("Diachronic", "Synchronic"),
  NumPlayers = c(2, 4),
  stringsAsFactors = FALSE
) %>%
  recode_strategy() %>%
  cbind(., predictSE(max_innovations_by_teamsize_mod, newdata = ., se = TRUE)) %>%
  rename(NumInnovations = fit, SE = se.fit) %>%
  recode_team_size()

set.seed(432)
max_innovations_by_teamsize_plot <- ggplot(Innovations) +
  aes(StrategyLabel, NumInnovations) +
  geom_bar(aes(fill = StrategyLabel), stat = "identity", alpha = 0.6,
           data = max_innovations_by_teamsize_preds) +
  geom_linerange(aes(ymin = NumInnovations-SE, ymax = NumInnovations+SE),
                 data = max_innovations_by_teamsize_preds) +
  geom_point(aes(color = StrategyLabel), shape = 1,
             position = position_jitter(width = 0.2, height = 0)) +
  facet_wrap("NumPlayersLabel") +
  scale_fill_manual(values = c(t_$synchronic_color, t_$diachronic_color)) +
  scale_color_manual(values = c(t_$synchronic_color, t_$diachronic_color)) +
  t_$base_theme +
  scale_y_continuous("Number of innovations", breaks = seq(0, 30, by = 2), expand = c(0, 0), limits = c(0, 27)) +
  # coord_cartesian(ylim)
  guides(color = "none", fill = "none") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  xlab("")

# BotsPlayers ----
data("BotsPlayers")

sim_vars <- c("strategy", "n_guesses", "n_players", "seed", "player_memory", "team_memory")
filter_final_round <- . %>%
  group_by_(.dots = sim_vars) %>%
  filter(round == max(round)) %>%
  ungroup()

min_players <- min(BotsPlayers$n_players)
max_players <- max(BotsPlayers$n_players)
min_guesses <- min(BotsPlayers$n_guesses)
max_guesses <- max(BotsPlayers$n_guesses)

BotsPlayersFinal <- BotsPlayers %>%
  filter((n_players == min_players & n_guesses == min_guesses) | (n_players == max_players & n_guesses == max_guesses)) %>%
  filter_final_round() %>%
  mutate(num_innovations = inventory_size - 6)

bots_team_size_plot <- ggplot(BotsPlayersFinal) +
  aes(strategy, num_innovations) +
  geom_bar(aes(fill = strategy), stat = "summary", fun.y = "mean", alpha = 0.6) +
  geom_point(aes(color = strategy), shape = 1, position = position_jitter(width = 0.2)) +
  facet_wrap("n_players")
