# ---- performance
inventory_mod <- lm(
  InventorySize ~ Diachronic_v_Synchronic + Diachronic_v_Isolated,
  data = team_performance
)

inventory_preds <- get_lm_mod_preds(inventory_mod) %>%
  rename(InventorySize = fit, SE = se.fit) %>%
  recode_strategy()

performance_plot <- ggplot(team_performance) +
  aes(StrategyLabel, InventorySize) +
  geom_point(aes(color = StrategyLabel),
             position = position_jitter(width = 0.3),
             alpha = 0.8) +
  geom_bar(aes(fill = StrategyLabel),
           stat = "summary", fun.y = "mean", alpha = 0.6) +
  geom_errorbar(aes(ymin = InventorySize - SE, ymax = InventorySize + SE, color = StrategyLabel),
                width = 0.2, size = 1.2, data = inventory_preds) +
  ylab("Number of inventions") +
  totems_theme["scale_x_strategy"] +
  totems_theme["scale_color_strategy"] +
  totems_theme["scale_fill_strategy"] +
  totems_theme["base_theme"] +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    axis.title.x = element_blank()
  )
performance_plot
