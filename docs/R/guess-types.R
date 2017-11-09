source("docs/R/setup.R")

# ---- guess-type-50
data("TeamPerformance")

TeamGuessTypes50 <- TeamPerformance %>%
  filter(
    TeamStatus == "V",
    Exp == "50LaborMinutes"
  ) %>%
  recode_strategy() %>%
  mutate(
    PropRedundantGuesses = NumRedundantGuesses/NumGuesses,
    PropRepeatedItems = NumRepeatedItems/NumGuesses,
    PropUniqueGuesses = NumUniqueGuesses/NumGuesses
  )

# Proportion of redundant guesses

prop_redundant_guesses_50_plot <- ggplot(TeamGuessTypes50) +
  aes(Strategy, PropRedundantGuesses,
      group = interaction(Strategy, SessionDuration)) +
  geom_bar(aes(fill = StrategyLabel), stat = "summary", fun.y = "mean",
           alpha = 0.6, position = position_dodge(width = 0.95)) +
  geom_point(aes(color = StrategyLabel, shape = factor(SessionDuration)),
             position = position_jitterdodge(dodge.width = 0.95, jitter.width = 0.2)) +
  scale_y_continuous("Redundant guesses", labels = scales::percent) +
  scale_shape_manual(values = c(16, 1)) +
  theme(legend.position = "none")

# Proportion of repeated items

prop_repeat_items_50_plot <- ggplot(TeamGuessTypes50) +
  aes(Strategy, PropRepeatedItems,
      group = interaction(Strategy, SessionDuration)) +
  geom_bar(aes(fill = StrategyLabel), stat = "summary", fun.y = "mean",
           alpha = 0.6, position = position_dodge(width = 0.95)) +
  geom_point(aes(color = StrategyLabel, shape = factor(SessionDuration)),
             position = position_jitterdodge(dodge.width = 0.95, jitter.width = 0.2)) +
  scale_y_continuous("Repeat items", labels = scales::percent) +
  scale_shape_manual(values = c(16, 1)) +
  theme(legend.position = "none")

# Proportion of unique guesses

prop_unique_guesses_50_plot <- ggplot(TeamGuessTypes50) +
  aes(Strategy, PropUniqueGuesses,
      group = interaction(Strategy, SessionDuration)) +
  geom_bar(aes(fill = StrategyLabel), stat = "summary", fun.y = "mean",
           alpha = 0.6, position = position_dodge(width = 0.95)) +
  geom_point(aes(color = StrategyLabel, shape = factor(SessionDuration)),
             position = position_jitterdodge(dodge.width = 0.95, jitter.width = 0.2)) +
  scale_y_continuous("Unique guesses", labels = scales::percent) +
  scale_shape_manual(values = c(16, 1)) +
  theme(legend.position = "none")

# Shared proportion of guesses

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
    GuessTypePropLabel = c("PropRedundantGuesses", "PropRepeatedItems", "PropUniqueGuesses", "PropUniqueItems"),
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

TeamGuessTypes50Means <- TeamGuessTypes50 %>%
  group_by(Strategy, SessionDuration) %>%
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
  select(-(NumGuesses:NumInnovations)) %>%
  gather(GuessTypePropLabel, PropGuesses, -(Strategy:SessionDuration)) %>%
  recode_prop_guess_type_total() %>%
  recode_strategy_by_session_duration()

prop_guess_types_plot <- ggplot(TeamGuessTypes50Means) +
  aes(StrategyBySessionDuration, PropGuesses) +
  geom_bar(aes(fill = GuessType), stat = "identity") +
  scale_x_discrete("Strategy by session duration") +
  scale_y_continuous("Proportion of guesses", labels = scales::percent) +
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(legend.position = "top")

# Distribution of guess types by generation

data("PlayerPerformance")

GuessTypesByGeneration50 <- PlayerPerformance %>%
  recode_strategy() %>%
  filter(
    TeamStatus == "V",
    Strategy != "Synchronic",
    SessionDuration == 25,
    Exp == "50LaborMinutes",
    NumGuesses < 400
  )  %>%
  group_by(Strategy, Generation) %>%
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
  select(Strategy, Generation, 
         PropRedundantGuesses, PropRepeatedItems, PropUniqueGuesses, PropUniqueItems) %>%
  gather(GuessTypePropLabel, PropGuesses, -(Strategy:Generation)) %>%
  recode_prop_guess_type_total()
  
prop_guess_types_by_generation_50_plot <- ggplot(GuessTypesByGeneration50) +
  aes(Generation, PropGuesses, fill = GuessType) +
  geom_bar(stat = "identity") +
  scale_x_continuous("Generation", breaks = 1:2) +
  scale_y_continuous("Proportion of guesses", labels = scales::percent) +
  facet_wrap("Strategy")
