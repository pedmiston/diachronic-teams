source("docs/R/_setup.R")

# ---- exp2

# Improvement over generations ----
data("PlayerPerformance")

NumInnovationsByGeneration100 <- PlayerPerformance %>%
  filter(
    TeamStatus == "V",
    Exp == "100LaborMinutes",
    Strategy != "Synchronic"
  ) %>%
  recode_strategy() %>%
  label_inheritance() %>%
  recode_inheritance() %>%
  recode_generation_type_100()

ggplot(NumInnovationsByGeneration100) +
  aes(Generation, NumInnovations, color = StrategyLabel) +
  geom_line(aes(group = TeamID), size = 0.4, alpha = 0.6) +
  geom_line(stat = "summary", fun.y = "mean", size = 1.4) +
  t_$scale_y_num_innovations +
  t_$scale_color_strategy +
  t_$base_theme

num_innovations_by_generation_100_mod <- lmer(
  NumInnovations ~ Diachronic_v_Isolated * GenerationC + (1|TeamID),
  data = NumInnovationsByGeneration100
)

num_innovations_by_generation_100_preds <- expand.grid(
  GenerationC = c(-0.5, 0.5),
  Strategy = c("Diachronic", "Isolated"),
  stringsAsFactors = FALSE
) %>%
  mutate(GenerationTypeLabel = c("Generation N", "Generation N+1", "Session N", "Session N+1")) %>%
  recode_strategy() %>%
  cbind(., predictSE(num_innovations_by_generation_100_mod, newdata = ., se = TRUE)) %>%
  rename(NumInnovations = fit, SE = se.fit)

num_innovations_by_generation_100_plot <- ggplot(NumInnovationsByGeneration100) +
  aes(GenerationTypeLabel, NumInnovations, color = StrategyLabel) +
  geom_line(aes(group = interaction(TeamID, GenerationTypeGroup)), alpha = 0.4) +
  geom_line(aes(group = 1), size = 1.5,
            stat = "identity", data = num_innovations_by_generation_100_preds) +
  geom_errorbar(aes(ymin = NumInnovations-SE, ymax = NumInnovations+SE),
                data = num_innovations_by_generation_100_preds,
                width = 0.1, size = 1.5) +
  scale_x_discrete("") +
  t_$scale_y_num_innovations +
  scale_color_manual(values = t_$color_picker(c("orange", "blue"))) +
  facet_wrap("Strategy", scales = "free_x") +
  t_$base_theme +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank())

num_innovations_by_generation_100_plot_grouped <- ggplot(NumInnovationsByGeneration100) +
  aes(GenerationTypeLabel, NumInnovations, color = StrategyLabel) +
  # geom_line(aes(group = interaction(TeamID, GenerationTypeGroup)), alpha = 0.4) +
  geom_line(aes(group = interaction(StrategyLabel, GenerationTypeGroup)), stat = "summary", fun.y = "mean")

# Unique innovations by generation ----
UniqueInnovations100 <- NumInnovationsByGeneration100 %>%
  group_by(TeamID, GenerationTypeGroup) %>%
  mutate(UniqueInnovations = ifelse(
    GenerationC == -0.5, NumInnovations[GenerationC == -0.5],
    NumInnovations[GenerationC == 0.5] - NumInnovations[GenerationC == -0.5]
  )
  ) %>%
  ungroup()

unique_innovations_100_mod <- lmer(
  UniqueInnovations ~ Diachronic_v_Isolated * GenerationC + (1|TeamID),
  data = UniqueInnovations100
)

unique_innovations_100_preds <- expand.grid(
  GenerationC = c(-0.5, 0.5),
  Strategy = c("Diachronic", "Isolated"),
  stringsAsFactors = FALSE
) %>%
  mutate(GenerationTypeLabel = c("Generation N", "Generation N+1", "Session N", "Session N+1")) %>%
  recode_strategy() %>%
  cbind(., predictSE(unique_innovations_100_mod, newdata = ., se = TRUE)) %>%
  rename(UniqueInnovations = fit, SE = se.fit)

num_unique_innovations_100_plot <- ggplot(UniqueInnovations100) +
  aes(GenerationTypeLabel, UniqueInnovations, color = StrategyLabel) +
  geom_line(aes(group = interaction(TeamID, GenerationTypeGroup)), alpha = 0.4) +
  geom_line(aes(group = 1), stat = "identity", size = 1.5,
            data = unique_innovations_100_preds) +
  geom_errorbar(aes(ymin = UniqueInnovations-SE, ymax = UniqueInnovations+SE),
                data = unique_innovations_100_preds,
                width = 0.1, size = 1.5) +
  scale_x_discrete("") +
  scale_y_continuous("Unique innovations", breaks = seq(-30, 30, by = 2)) +
  scale_color_manual(values = t_$color_picker(c("orange", "blue"))) +
  facet_wrap("Strategy", scales = "free_x") +
  t_$base_theme +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank())


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
