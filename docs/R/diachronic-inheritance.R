source("docs/R/setup.R")

# ---- inheritance
data("Guesses")
data("Players")
data("AdjacentItems")

IndividualPlayers <- filter(Players, Strategy != "Synchronic", Exp == "100LaborMinutes", TeamStatus == "V")

IndividualGuesses <- Guesses %>%
  filter(Strategy != "Synchronic", Exp == "100LaborMinutes", TeamStatus == "V") %>%
  left_join(AdjacentItems, by = c("PrevSessionInventoryID" = "ID"))

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

ggplot(IndividualStages) +
  aes(Stage, NumGuesses, color = Inheritance) +
  geom_line(aes(group = Inheritance),
            stat = "identity", data = individual_stages_preds) +
  geom_errorbar(aes(ymin = NumGuesses-SE, ymax = NumGuesses+SE),
                data = individual_stages_preds,
                width = 0.2)

individual_stages_mod_plot <- individual_stages_plot +
  geom_smooth(aes(ymin = NumGuesses - SE, ymax = NumGuesses + SE),
              stat = "identity", data = individual_stages_preds)

FirstDiscovery <- IndividualStages %>%
  filter(StageIX == 0, Inheritance != "no_inheritance") %>%
  recode_strategy()

first_discovery_mod <- lm(NumGuesses ~ Diachronic_v_Isolated,
                          data = FirstDiscovery)

first_discovery_preds <- recode_strategy() %>%
  filter(Strategy != "Synchronic") %>%
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
           alpha = 0.6) +
  geom_point(aes(color = StrategyLabel),
             position = position_jitter(width = 0.2)) +
  facet_wrap("StrategyLabel") +
  t_$scale_fill_strategy +
  t_$scale_color_strategy +
  t_$base_theme +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  ggtitle("Cost of first innovation")
