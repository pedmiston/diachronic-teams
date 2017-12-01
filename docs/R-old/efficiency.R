source("docs/R/setup.R")

# ---- efficiency
Efficiency <- Inventories %>%
  left_join(TeamInfo) %>%
  recode_strategy()

efficiency_mod <- lmer(
  TeamGuesses ~ Diachronic_v_Synchronic + Diachronic_v_Isolated + (1|TeamID) + (1|TeamInventory),
  data = Efficiency
)

efficiency_preds <- recode_strategy() %>%
  cbind(., AICcmodavg::predictSE(efficiency_mod, newdata = ., se = TRUE)) %>%
  rename(Guesses = fit, SE = se.fit) %>%
  recode_strategy()

EfficiencyMeans <- Efficiency %>%
  group_by(TeamID) %>%
  summarize(Guesses = mean(TeamGuesses)) %>%
  left_join(TeamInfo) %>%
  recode_strategy()

set.seed(342)
efficiency_plot <- ggplot(EfficiencyMeans) +
  aes(StrategyLabel, Guesses, color = StrategyLabel) +
  geom_point(position = position_jitter(width = 0.15), shape = 1) +
  geom_errorbar(aes(ymin = Guesses - SE, ymax = Guesses + SE),
                data = efficiency_preds, width = 0.1, size = 1.1) +
  t_["scale_x_strategy"] +
  scale_y_reverse("Guesses per invention", breaks = c(1, seq(50, 250, by = 50))) +
  t_["scale_color_strategy"] +
  scale_shape_manual("", values = c(16, 1)) +
  t_["scale_fill_strategy"] +
  t_["base_theme"] +
  coord_cartesian(ylim = c(0, 210)) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )
