source("docs/simulations/R/setup.R")

# ---- strategy
data("BotsStrategy")

timeline_plot <- ggplot(BotsStrategy) +
  aes(round, inventory_size, color = strategy) +
  geom_line(stat = "summary", fun.y = "mean", size = 1.2) +
  scale_x_continuous("Round") +
  totems_theme$scale_y_inventory_size +
  scale_color_manual(values = totems_theme$color_picker(c("blue", "green"))) +
  coord_cartesian(ylim = c(6, 16)) +
  totems_theme$base_theme +
  theme(legend.position = "top")

BotsStrategyFinal <- BotsStrategy %>%
  filter_final_round() %>%
  mutate(strategy_rev = factor(strategy, levels = c("synchronic", "diachronic")))

final_plot <- ggplot(BotsStrategyFinal) +
  aes(strategy_rev, inventory_size) +
  geom_bar(aes(fill = strategy), stat = "summary", fun.y = "mean",
           alpha = 0.6) +
  geom_point(aes(color = strategy), position = position_jitter(0.3),
             shape = 1) +
  scale_x_discrete("Strategy", labels = c("Synchronic", "Diachronic")) +
  totems_theme$scale_y_inventory_size +
  scale_color_manual(values = totems_theme$color_picker(c("blue", "green"))) +
  scale_fill_manual(values = totems_theme$color_picker(c("blue", "green"))) +
  coord_cartesian(ylim = c(6, 16)) +
  totems_theme$base_theme +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )

gridExtra::grid.arrange(
  crotchet::read_graphviz("labor-hours", "totems"),
  timeline_plot + theme(legend.position = "none"),
  final_plot + theme(axis.title.y = element_blank()),
  nrow = 1,
  widths = c(0.25, 0.375, 0.375)
)