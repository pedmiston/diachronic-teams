source("docs/R/setup.R")

# ---- cost-per-item
data("Guesses")
data("AdjacentItems")
data("Teams")

CostPerItem <- Guesses %>%
  group_by(SessionID) %>%
  mutate(GuessTime = SessionTime - lag(SessionTime, default = 0)) %>%
  ungroup() %>%
  # Copy guesses for each adjacent item
  left_join(AdjacentItems, by = c("PrevSessionInventoryID" = "ID")) %>%
  group_by(Exp, TeamID, Adjacent) %>%
  summarize(
    TotalGuesses = n(),
    TotalTime = sum(GuessTime, na.rm = TRUE),
    Discovered = any(Result == Adjacent)
  ) %>%
  ungroup() %>%
  left_join(Teams) %>%
  recode_discovered() %>%
  recode_strategy()

scale_x_adjacent <- scale_x_discrete("Innovation ID")

# 50: Guesses per item ----
TeamCostPerItem50 <- CostPerItem %>%
  filter(Exp == "50LaborMinutes", TeamStatus == "V") %>%
  mutate(AdjacentFactor = factor(Adjacent))

guesses_per_item_50_plot <- ggplot(TeamCostPerItem50) +
  aes(AdjacentFactor, TotalGuesses, group = Strategy) +
  geom_bar(aes(fill = StrategyLabel),
           stat = "summary", fun.y = "mean", position = "dodge") +
  scale_x_adjacent +
  totems_theme$scale_fill_strategy +
  totems_theme$base_theme +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(legend.position = "top")

guesses_per_item_50_mod <- lmer(TotalGuesses ~ Diachronic_v_Isolated + Diachronic_v_Synchronic +
                                  (Diachronic_v_Isolated + Diachronic_v_Synchronic|Adjacent),
                                data = TeamCostPerItem50)

guesses_per_item_50_preds <- recode_strategy() %>%
  cbind(., predictSE(guesses_per_item_50_mod, newdata = ., se = TRUE)) %>%
  rename(TotalGuesses = fit, SE = se.fit)

guesses_per_item_50_mod_plot <- ggplot(guesses_per_item_50_preds) +
  aes(StrategyLabel, TotalGuesses) +
  geom_bar(aes(fill = StrategyLabel), stat = "identity") +
  geom_linerange(aes(ymin = TotalGuesses - SE, ymax = TotalGuesses + SE)) +
  scale_y_continuous("Average guesses per item") +
  totems_theme$scale_x_strategy +
  totems_theme$scale_fill_strategy +
  totems_theme$base_theme +
  theme(legend.position = "none")

# 50: Guesses per item by discovery ----
guesses_per_item_50_by_discovery_plot <- guesses_per_item_50_plot +
  facet_wrap("DiscoveredLabel", nrow = 2, scales = "free_y")

guesses_per_discovered_item_50_mod <- lmer(
  TotalGuesses ~ Diachronic_v_Isolated + Diachronic_v_Synchronic +
    (Diachronic_v_Isolated + Diachronic_v_Synchronic|Adjacent),
  data = filter(TeamCostPerItem50, Discovered)
)
guesses_per_discovered_item_50_mod_preds <- recode_strategy() %>%
  cbind(., predictSE(guesses_per_discovered_item_50_mod, newdata = ., se = TRUE)) %>%
  rename(TotalGuesses = fit, SE = se.fit)

guesses_per_undiscovered_item_50_mod <- lmer(
  TotalGuesses ~ Diachronic_v_Isolated + Diachronic_v_Synchronic +
    (Diachronic_v_Isolated + Diachronic_v_Synchronic|Adjacent),
  data = filter(TeamCostPerItem50, !Discovered)
)
guesses_per_undiscovered_item_50_mod_preds <- recode_strategy() %>%
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
  totems_theme$scale_x_strategy +
  totems_theme$scale_fill_strategy +
  totems_theme$base_theme +
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
  totems_theme$scale_x_strategy +
  totems_theme$scale_fill_strategy +
  totems_theme$base_theme +
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
