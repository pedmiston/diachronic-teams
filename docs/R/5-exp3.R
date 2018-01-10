source("docs/R/0-setup.R")
source("docs/R/1-intro.R")
source("docs/R/2-methods.R")
# ---- exp3

# Methods ----
data("Sessions")

Exp3Participants <- Sessions %>%
  filter_exp3() %>%
  select(Strategy, PlayerID) %>%
  unique() %>%
  count(Strategy) %>%
  rename(N = n)

# Innovations by generation ----
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

# Cost by stage ----
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
