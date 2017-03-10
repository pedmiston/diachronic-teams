source("docs/R/setup.R")

# ---- efficiency
efficiency_mod <- lmer(
  Guesses ~
    Diachronic_v_Synchronic + Diachronic_v_Isolated +
    (Diachronic_v_Synchronic + Diachronic_v_Isolated|TeamInventory) +
    NumTeamInnovations +
    (1|TeamID),
  data = TeamInventoryGuesses
)

efficiency_preds <- expand.grid(
  Strategy = recode_strategy()$Strategy,
  NumTeamInnovations = mean(TeamInventoryGuesses$NumTeamInnovations),
  stringsAsFactors = FALSE
) %>%
  recode_strategy() %>%
  cbind(., AICcmodavg::predictSE(efficiency_mod, newdata = ., se = TRUE)) %>%
  rename(Guesses = fit, SE = se.fit)

efficiency_plot <- ggplot(efficiency_preds) +
  aes(StrategyLabel, Guesses) +
  geom_bar(aes(fill = StrategyLabel), stat = "identity",
           alpha = 0.6) +
  geom_errorbar(aes(ymin = Guesses - SE, ymax = Guesses + SE),
                width = 0.3) +
  totems_theme["scale_x_strategy"] +
  ylab("Guesses per invention") +
  totems_theme["scale_fill_strategy"] +
  totems_theme["base_theme"] +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )
