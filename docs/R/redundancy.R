source("docs/R/setup.R")

# ---- redundancy
redundancy_mod <- lmer(
  Redundancy ~ Diachronic_v_Synchronic + Diachronic_v_Isolated + (1|TeamID),
  data = TeamInventoryGuesses
)

redundancy_preds <- recode_strategy() %>%
  cbind(., AICcmodavg::predictSE(redundancy_mod, newdata = ., se = TRUE)) %>%
  rename(Redundancy = fit, SE = se.fit)

redundancy_plot <- ggplot(redundancy_preds) +
  aes(StrategyLabel, Redundancy) +
  geom_bar(aes(fill = StrategyLabel), stat = "identity", alpha = 0.6) +
  geom_errorbar(aes(ymin = Redundancy - SE, ymax = Redundancy + SE),
                width = 0.2) +
  coord_cartesian(ylim = c(0, 1)) +
  totems_theme["scale_x_strategy"] +
  scale_y_continuous("Redundancy", labels = scales::percent) +
  totems_theme["scale_fill_strategy"] +
  totems_theme["base_theme"] +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )
