source("docs/R/setup.R")
# source("docs/R/axis-pics.R")

# ---- cost-per-item
data("Guesses")
data("AdjacentItems")
data("Teams")

Players50min <- Players %>%
  filter(Exp == "50LaborMinutes", TeamStatus == "V")

CostPerItem50min <- Guesses %>%
  filter(Exp == "50LaborMinutes", TeamStatus == "V") %>%
  # Copy guesses for each adjacent item
  left_join(AdjacentItems, by = c("PrevSessionInventoryID" = "ID")) %>%
  mutate(AdjacentFactor = factor(Adjacent)) %>%
  filter(Stage == "playing") %>%
  group_by(SessionID, Adjacent) %>%
  summarize(
    TotalGuesses = n(),
    TotalTime = sum(GuessTime, na.rm = TRUE),
    Discovered = any(Result == Adjacent)
  ) %>%
  ungroup() %>%
  left_join(Players50min) %>%
  group_by(Strategy) %>%
  do({
    if(.data$Strategy[[1]] == "Synchronic") {
      results <- .data %>%
        group_by(TeamID) %>%
        summarize(
          TotalGuesses = sum(TotalGuesses),
          TotalTime = sum(TotalTime),
          Discovered = any(Discovered)
        )
    } else {
      results <- .data
    }
    results
  }) %>%
  ungroup() %>%
  filter(Discovered) %>%
  left_join(Players50min) %>%
  recode_strategy() %>%
  filter(!(Strategy == "Diachronic" & Generation == 1)) %>%
  recode_session_type_50min()

guesses_per_item_50_plot <- ggplot(CostPerItem50min) +
  aes(factor(Adjacent), TotalGuesses, group = SessionTypeSimple) +
  geom_bar(aes(fill = Strategy),
           stat = "summary", fun.y = "mean", position = "dodge")

guesses_per_item_50_mod <- lmer(
  TotalGuesses ~ Diachronic_v_Isolated + Diachronic_v_Synchronic +
    (Diachronic_v_Isolated + Diachronic_v_Synchronic|Adjacent)+
    (1|TeamID),
  data = CostPerItem50min)

guesses_per_item_50_preds <- recode_strategy() %>%
  cbind(., predictSE(guesses_per_item_50_mod, newdata = ., se = TRUE)) %>%
  rename(TotalGuesses = fit, SE = se.fit)

guesses_per_item_50_mod_plot <- ggplot(guesses_per_item_50_preds) +
  aes(StrategyLabel, TotalGuesses) +
  geom_bar(aes(fill = StrategyLabel), stat = "identity") +
  geom_linerange(aes(ymin = TotalGuesses - SE, ymax = TotalGuesses + SE)) +
  scale_y_continuous("Average guesses per item") +
  t_$scale_fill_strategy +
  t_$base_theme +
  theme(legend.position = "none")

# 50: Guesses per item by discovery ----
guesses_per_item_50_by_discovery_plot <- guesses_per_item_50_plot +
  facet_wrap("DiscoveredLabel", nrow = 2, scales = "free_y")

guesses_per_discovered_item_50_mod <- lmer(
  TotalGuesses ~ Diachronic_v_Individual + Diachronic_v_NoInheritance +
    (Diachronic_v_Individual + Diachronic_v_NoInheritance|Adjacent),
  data = filter(TeamCostPerItem50, Discovered)
)
guesses_per_discovered_item_50_mod_preds <- recode_inheritance() %>%
  select(-Generation, -SessionDuration, -Strategy) %>%
  unique() %>%
  cbind(., predictSE(guesses_per_discovered_item_50_mod, newdata = ., se = TRUE)) %>%
  rename(TotalGuesses = fit, SE = se.fit)

guesses_per_undiscovered_item_50_mod <- lmer(
  TotalGuesses ~ Diachronic_v_Individual + Diachronic_v_NoInheritance +
    (Diachronic_v_Individual + Diachronic_v_NoInheritance|Adjacent),
  data = filter(TeamCostPerItem50, !Discovered)
)
guesses_per_undiscovered_item_50_mod_preds <- recode_inheritance() %>%
  select(-Generation, -SessionDuration, -Strategy) %>%
  unique() %>%
  cbind(., predictSE(guesses_per_undiscovered_item_50_mod, newdata = ., se = TRUE)) %>%
  rename(TotalGuesses = fit, SE = se.fit)

guesses_per_item_50_by_discovered_preds <- bind_rows(
  discovered = guesses_per_discovered_item_50_mod_preds,
  undiscovered = guesses_per_undiscovered_item_50_mod_preds,
  .id = 'DiscoveredType'
) %>%
  recode_discovered()

guesses_per_item_50_by_discovered_mod_plot <- (guesses_per_item_50_mod_plot %+% guesses_per_item_50_by_discovered_preds) +
  facet_wrap("DiscoveredLabel", ncol = 1, scales = "free_y")

# 50: Time per item ----
time_per_item_50_plot <- guesses_per_item_50_plot +
  aes(y = TotalTime)

time_per_item_50_mod <- lmer(
  TotalTime ~ Diachronic_v_Isolated + Diachronic_v_Synchronic +
    (Diachronic_v_Isolated + Diachronic_v_Synchronic|Adjacent) +
    (1|TeamID),
  data = TeamCostPerItem50
)

time_per_item_50_preds <- recode_strategy() %>%
  cbind(., predictSE(time_per_item_50_mod, newdata = ., se = TRUE)) %>%
  rename(TotalTime = fit, SE = se.fit)

time_per_item_50_mod_plot <- ggplot(time_per_item_50_preds) +
  aes(StrategyLabel, TotalTime) +
  geom_bar(aes(fill = StrategyLabel), stat = "identity") +
  geom_linerange(aes(ymin = TotalTime - SE, ymax = TotalTime + SE)) +
  scale_y_continuous("Labor time per item") +
  t_$scale_x_strategy +
  t_$scale_fill_strategy +
  t_$base_theme +
  theme(legend.position = "none")

# 50: Time per item by discovery
time_per_item_50_by_discovery_plot <- time_per_item_50_plot +
  facet_wrap("DiscoveredLabel", nrow = 2, scales = "free_y")

time_per_discovered_item_50_mod <- lmer(
  TotalGuesses ~ Diachronic_v_Isolated + Diachronic_v_Synchronic +
    (Diachronic_v_Isolated + Diachronic_v_Synchronic|Adjacent),
  data = filter(TeamCostPerItem50, Discovered)
)
time_per_discovered_item_50_mod_preds <- recode_strategy() %>%
  cbind(., predictSE(time_per_discovered_item_50_mod, newdata = ., se = TRUE)) %>%
  rename(TotalTime = fit, SE = se.fit)

time_per_undiscovered_item_50_mod <- lmer(
  TotalGuesses ~ Diachronic_v_Isolated + Diachronic_v_Synchronic +
    (Diachronic_v_Isolated + Diachronic_v_Synchronic|Adjacent),
  data = filter(TeamCostPerItem50, !Discovered)
)
time_per_undiscovered_item_50_mod_preds <- recode_strategy() %>%
  cbind(., predictSE(time_per_undiscovered_item_50_mod, newdata = ., se = TRUE)) %>%
  rename(TotalTime = fit, SE = se.fit)

time_per_item_50_by_discovered_preds <- bind_rows(
  discovered = time_per_discovered_item_50_mod_preds,
  undiscovered = time_per_undiscovered_item_50_mod_preds,
  .id = 'DiscoveredType'
) %>%
  recode_discovered()

time_per_item_50_by_discovered_mod_plot <- (time_per_item_50_mod_plot %+% time_per_item_50_by_discovered_preds) +
  facet_wrap("DiscoveredLabel", ncol = 1, scales = "free_y")

# 100: Guesses per item ----
TeamCostPerItem100 <- CostPerItem %>%
  filter(Exp == "100LaborMinutes", TeamStatus == "V") %>%
  filter(!is.na(Adjacent), !is.na(Discovered)) %>%
  mutate(AdjacentFactor = factor(Adjacent))

guesses_per_item_100_plot <- (guesses_per_item_50_plot %+% TeamCostPerItem100)

guesses_per_item_100_mod <- lmer(
  TotalGuesses ~ Diachronic_v_Isolated + Diachronic_v_Synchronic +
    (Diachronic_v_Isolated + Diachronic_v_Synchronic|Adjacent),
  data = filter(TeamCostPerItem100, Discovered == TRUE))

guesses_per_item_100_preds <- recode_strategy() %>%
  cbind(., predictSE(guesses_per_item_100_mod, newdata = ., se = TRUE)) %>%
  rename(TotalGuesses = fit, SE = se.fit)

guesses_per_item_100_mod_plot <- ggplot(guesses_per_item_100_preds) +
  aes(StrategyLabel, TotalGuesses) +
  geom_bar(aes(fill = StrategyLabel), stat = "identity") +
  geom_linerange(aes(ymin = TotalGuesses - SE, ymax = TotalGuesses + SE)) +
  scale_y_continuous("Average guesses per item") +
  t_$scale_x_strategy +
  t_$scale_fill_strategy +
  t_$base_theme +
  theme(legend.position = "none")

# 100: Guesses per item by discovered ----
guesses_per_item_100_by_discovery_plot <- guesses_per_item_100_plot +
  facet_wrap("DiscoveredLabel", nrow = 2, scales = "free_y")

guesses_per_discovered_item_100_mod <- lmer(
  TotalGuesses ~ Diachronic_v_Isolated + Diachronic_v_Synchronic +
    (Diachronic_v_Isolated + Diachronic_v_Synchronic|Adjacent),
  data = filter(TeamCostPerItem100, Discovered)
)
guesses_per_discovered_item_100_mod_preds <- recode_strategy() %>%
  cbind(., predictSE(guesses_per_discovered_item_100_mod, newdata = ., se = TRUE)) %>%
  rename(TotalGuesses = fit, SE = se.fit)

guesses_per_undiscovered_item_100_mod <- lmer(
  TotalGuesses ~ Diachronic_v_Isolated + Diachronic_v_Synchronic +
    (Diachronic_v_Isolated + Diachronic_v_Synchronic|Adjacent),
  data = filter(TeamCostPerItem100, !Discovered)
)
guesses_per_undiscovered_item_100_mod_preds <- recode_strategy() %>%
  cbind(., predictSE(guesses_per_undiscovered_item_100_mod, newdata = ., se = TRUE)) %>%
  rename(TotalGuesses = fit, SE = se.fit)

guesses_per_item_100_by_discovered_preds <- bind_rows(
  discovered = guesses_per_discovered_item_100_mod_preds,
  undiscovered = guesses_per_undiscovered_item_100_mod_preds,
  .id = 'DiscoveredType'
) %>%
  recode_discovered()

guesses_per_item_100_by_discovered_mod_plot <- (guesses_per_item_100_mod_plot %+% guesses_per_item_100_by_discovered_preds) +
  facet_wrap("DiscoveredLabel", ncol = 1, scales = "free_y")
