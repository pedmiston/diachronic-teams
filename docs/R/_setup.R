devtools::load_all("~/Research/DiachronicTeams/data")

# ---- setup
library(tidyverse)
library(magrittr)
library(broom)
library(lme4)
library(AICcmodavg)
library(gridExtra)
library(crotchet)
library(totems)
t_ <- load_totems_theme()

# Types of time ----
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

# Cost per item ----
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

# Counts ----
data("Players")

TeamCounts <- Teams %>%
  filter(
    TeamStatus == "V",
    !(Strategy == "Isolated" & SessionsPerPlayer == 2),
    !(Strategy == "Diachronic" & NumPlayers == 2)
  ) %>%
  rename(TeamSize = NumPlayers) %>%
  count(Strategy, SessionDuration, TeamSize) %>%
  rename(NumTeams = n)

PlayerCounts <- Players %>%
  left_join(
    Teams %>%
      filter(
        TeamStatus == "V",
        !(Strategy == "Isolated" & SessionsPerPlayer == 2),
        !(Strategy == "Diachronic" & NumPlayers == 2)
      ) %>%
      select(TeamID, SessionDuration, TeamSize = NumPlayers, SessionsPerPlayer, PlayersPerSession) %>%
      unique()
  ) %>%
  filter(
    TeamStatus == "V",
    !(Strategy == "Isolated" & SessionsPerPlayer == 2),
    !(Strategy == "Diachronic" & NumPlayers == 2)
  ) %>%
  select(PlayerID, Strategy, SessionDuration, TeamSize, SessionsPerPlayer, PlayersPerSession) %>%
  unique() %>%
  count(Strategy, SessionDuration, TeamSize) %>%
  rename(NumPlayers = n)

ConditionCounts <- Teams %>%
  filter(
    TeamStatus == "V",
    !(Strategy == "Isolated" & SessionsPerPlayer == 2),
    !(Strategy == "Diachronic" & NumPlayers == 2)
  ) %>%
  select(Strategy, SessionDuration, TeamSize = NumPlayers, SessionsPerPlayer, PlayersPerSession) %>%
  unique() %>%
  arrange(Strategy) %>%
  left_join(TeamCounts) %>%
  left_join(PlayerCounts)

TeamCounts50 <- Teams %>%
  filter(TeamStatus == "V", Exp == "50LaborMinutes") %>%
  rename(TeamSize = NumPlayers) %>%
  count(Strategy, SessionDuration, TeamSize) %>%
  rename(NumTeams = n)

PlayerCounts50 <- Players %>%
  left_join(
    Teams %>%
      filter(TeamStatus == "V", Exp == "50LaborMinutes") %>%
      select(TeamID, SessionDuration, TeamSize = NumPlayers, SessionsPerPlayer, PlayersPerSession) %>%
      unique()
  ) %>%
  filter(TeamStatus == "V", Exp == "50LaborMinutes") %>%
  select(PlayerID, Strategy, SessionDuration, TeamSize, SessionsPerPlayer, PlayersPerSession) %>%
  unique() %>%
  count(Strategy, SessionDuration, TeamSize) %>%
  rename(NumPlayers = n)

ConditionCounts50 <- Teams %>%
  filter(TeamStatus == "V", Exp == "50LaborMinutes") %>%
  select(Strategy, SessionDuration, TeamSize = NumPlayers, SessionsPerPlayer, PlayersPerSession) %>%
  unique() %>%
  arrange(Strategy) %>%
  left_join(TeamCounts50) %>%
  left_join(PlayerCounts50)

TeamCounts100 <- Teams %>%
  filter(TeamStatus == "V", Exp == "100LaborMinutes") %>%
  rename(TeamSize = NumPlayers) %>%
  count(Strategy, TeamSize) %>%
  rename(NumTeams = n)

PlayerCounts100 <- Players %>%
  left_join(
    Teams %>%
      filter(TeamStatus == "V", Exp == "100LaborMinutes") %>%
      select(TeamID, TeamSize = NumPlayers, SessionsPerPlayer, PlayersPerSession) %>%
      unique()
  ) %>%
  filter(TeamStatus == "V", Exp == "100LaborMinutes") %>%
  select(PlayerID, Strategy, TeamSize, SessionsPerPlayer, PlayersPerSession) %>%
  unique() %>%
  count(Strategy, TeamSize) %>%
  rename(NumPlayers = n)

ConditionCounts100 <- Teams %>%
  filter(TeamStatus == "V", Exp == "100LaborMinutes") %>%
  select(Strategy, SessionDuration, TeamSize = NumPlayers, SessionsPerPlayer, PlayersPerSession) %>%
  unique() %>%
  arrange(Strategy) %>%
  left_join(TeamCounts100) %>%
  left_join(PlayerCounts100)


# Combinations ----
count_unique_guesses <- function(n_items) {
  n_guesses_of_size <- function(guess_size) {
    nrow(expand.grid(rep(list(1:n_items), guess_size)))
  }
  sum(sapply(1:4, n_guesses_of_size))
}

# Guess types ----
recode_guess_type_total <- function(frame) {
  guess_type_total_map <- data_frame(
    GuessTypeTotal = c("NumRedundantGuesses", "NumRepeatedItems", "NumUniqueGuesses", "NumInnovations"),
    GuessType = c("redundant", "repeat_item", "unique_guess", "unique_result")
  )
  if(missing(frame)) return(guess_type_total_map)
  left_join(frame, guess_type_total_map)
}


recode_prop_guess_type_total <- function(frame) {
  prop_guess_type_total_map <- data_frame(
    PropGuessType = c("PropRedundantGuesses", "PropRepeatedItems", "PropUniqueGuesses", "PropUniqueItems"),
    PropGuessTypeLabel = c("Redundant", "Repeat item", "Unique guess", "Unique item"),
    GuessType = c("redundant", "repeat_item", "unique_guess", "unique_result")
  )
  if(missing(frame)) return(prop_guess_type_total_map)
  left_join(frame, prop_guess_type_total_map)
}

recode_strategy_by_session_duration <- function(frame) {
  strategy_by_session_duration_map <- data_frame(
    Strategy = c("Diachronic", "Synchronic", "Isolated", "Isolated"),
    SessionDuration = c(25, 25, 25, 50),
    StrategyBySessionDuration = c("Diachronic-25", "Synchronic-25", "Isolated-25", "Isolated-50")
  )
  if(missing(frame)) return(strategy_by_session_duration_map)
  left_join(frame, strategy_by_session_duration_map)
}
