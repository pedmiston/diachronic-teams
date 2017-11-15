source("docs/R/setup.R")

# ---- inheritance
data("Guesses")
data("Players")

IndividualPlayers <- filter(Players, Strategy != "Synchronic", Exp == "100LaborMinutes", TeamStatus == "V")

IndividualGuesses <- Guesses %>%
  filter(Strategy != "Synchronic", Exp == "100LaborMinutes", TeamStatus == "V") %>%
  label_stage_ix()

IndividualStages <- IndividualGuesses %>%
  group_by(SessionID, StageIX) %>%
  summarize(NumGuesses = n()) %>%
  left_join(IndividualPlayers) %>%
  highlight_inheritance_100() %>%
  # Should not be necessary!
  filter(!is.na(Inheritance))

IndividualStagesModData <- IndividualStages %>%
  filter(StageIX >= 0) %>%
  mutate(StageIX_2 = StageIX^2)

individual_stages_mod <- lmer(
  NumGuesses ~ Diachronic_v_Individual * (StageIX + StageIX_2) +
    (StageIX + StageIX_2|PlayerID),
  data = filter(IndividualStagesModData, Inheritance != "no_inheritance")
)

inheritance_labels <- highlight_inheritance_100() %>%
  select(-Generation) %>%
  unique() %>%
  filter(Strategy != "Synchronic")

individual_stages_preds <- expand.grid(
  StageIX = 0:10,
  Inheritance = c("diachronic_inheritance", "individual_inheritance"),
  stringsAsFactors = FALSE
) %>%
  mutate(StageIX_2 = StageIX^2) %>%
  left_join(inheritance_labels) %>%
  mutate(StageIX_2 = StageIX^2) %>%
  cbind(., predictSE(individual_stages_mod, newdata = ., se = TRUE)) %>%
  rename(NumGuesses = fit, SE = se.fit)

individual_stages_plot <- ggplot(IndividualStages) +
  aes(StageIX, NumGuesses, color = InheritanceOrdered) +
  geom_line(aes(group = InheritanceOrdered),
            stat = "summary", fun.y = "mean") +
  scale_x_continuous("Size of inventory relative to ancestor") +
  scale_y_continuous("Number of guesses") +
  totems_theme$scale_color_strategy +
  totems_theme$base_theme +
  theme(legend.position = c(0.85, 0.8)) +
  ggtitle("Diachronic inheritance")

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
  totems_theme$scale_color_strategy +
  totems_theme$scale_fill_strategy +
  totems_theme$base_theme +
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
  totems_theme$scale_fill_strategy +
  totems_theme$scale_color_strategy +
  totems_theme$base_theme +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  ggtitle("Cost of first innovation")
