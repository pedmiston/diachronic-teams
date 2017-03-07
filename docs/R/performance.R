# ---- performance
score_mod <- lm(
  Score ~ Diachronic_v_Synchronic + Diachronic_v_Isolated,
  data = totems_teams
)

inventory_mod <- lm(
  InventorySize ~ Diachronic_v_Synchronic + Diachronic_v_Isolated,
  data = totems_teams
)

difficulty_mod <- lm(
  DifficultyScore ~ Diachronic_v_Synchronic + Diachronic_v_Isolated,
  data = totems_teams
)

performance_preds <- list(
    Score = get_lm_mod_preds(score_mod),
    InventorySize = get_lm_mod_preds(inventory_mod),
    DifficultyScore = get_lm_mod_preds(difficulty_mod)
  ) %>%
  bind_rows(.id = "Measure") %>%
  rename(Value = fit, SE = se.fit) %>%
  recode_score_value() %>%
  recode_strategy()

performance_plot <- ggplot(team_ratchets) +
  aes(StrategyLabel, Value) +
  geom_point(aes(color = StrategyLabel),
             position = position_jitter(width = 0.3)) +
  geom_bar(aes(fill = StrategyLabel),
           stat = "summary", fun.y = "mean", alpha = 0.6) +
  geom_linerange(aes(ymin = Value - SE, ymax = Value + SE),
                 data = performance_preds) +
  facet_wrap("MeasureLabel", scales = "free_y", strip.position = "left") +
  totems_theme["scale_x_strategy"] +
  totems_theme["scale_color_strategy"] +
  totems_theme["scale_fill_strategy"] +
  totems_theme["base_theme"] +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    strip.placement = "outside",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
performance_plot
