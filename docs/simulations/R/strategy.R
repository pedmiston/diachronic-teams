source("docs/simulations/R/setup.R")

# ---- strategy
data("BotsStrategy")

BotsStrategy %<>%
  mutate(strategy_rev = factor(strategy, levels = c("synchronic", "diachronic")))

timeline_plot <- ggplot(BotsStrategy) +
  aes(round, inventory_size, color = strategy) +
  geom_line(stat = "summary", fun.y = "mean", size = 1.2) +
  geom_vline(xintercept = 49, linetype = 2, size = 0.3) +
  annotate("text", x = 49, y = 14, label = "calendar hours", angle = 90,
           vjust = -0.2, size = 3) +
  scale_x_continuous("Round") +
  totems_theme$scale_y_inventory_size +
  totems_theme$scale_color_strategy_2 +
  coord_cartesian(ylim = c(6, 16)) +
  totems_theme$base_theme +
  theme(legend.position = "top")

final_plot <- ggplot() +
  aes(strategy_rev, inventory_size) +
  geom_bar(aes(fill = strategy), stat = "summary", fun.y = "mean",
           alpha = 0.6) +
  geom_point(aes(color = strategy), position = position_jitter(0.3),
             shape = 1) +
  scale_x_discrete("Strategy", labels = c("Synchronic", "Diachronic")) +
  totems_theme$scale_y_inventory_size +
  totems_theme$scale_color_strategy_2 +
  totems_theme$scale_fill_strategy_2 +
  coord_cartesian(ylim = c(6, 16)) +
  totems_theme$base_theme +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )

calendar_hours_plot <- final_plot %+% filter(BotsStrategy, round == 49)
labor_hours_plot    <- final_plot %+% filter_final_round(BotsStrategy)

gridExtra::grid.arrange(
  crotchet::read_graphviz("team-structures-2", "totems"),
  timeline_plot +
    theme(legend.position = "none") +
    ggtitle("Performance over time"),
  calendar_hours_plot +
    theme(axis.title.y = element_blank()) +
    ggtitle("Calendar hours"),
  labor_hours_plot +
    theme(axis.title.y = element_blank()) +
    ggtitle("Labor hours"),
  nrow = 1,
  widths = c(0.16, 0.28, 0.28, 0.28)
)
