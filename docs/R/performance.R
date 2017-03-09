# ---- performance
inventory_mod <- lm(
  NumInnovations ~ Diachronic_v_Synchronic + Diachronic_v_Isolated,
  data = TotemsTeams
)

inventory_preds <- get_lm_mod_preds(inventory_mod) %>%
  rename(NumInnovations = fit, SE = se.fit) %>%
  recode_strategy()

performance_plot <- ggplot(TotemsTeams) +
  aes(StrategyLabel, NumInnovations) +
  geom_point(aes(color = StrategyLabel),
             position = position_jitter(width = 0.3),
             alpha = 0.8) +
  geom_bar(aes(fill = StrategyLabel),
           stat = "summary", fun.y = "mean", alpha = 0.6) +
  geom_errorbar(aes(ymin = NumInnovations - SE, ymax = NumInnovations + SE, color = StrategyLabel),
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

sampled_inventories <- TotemsTrials %>%
  group_by(TeamID) %>%
  do({ get_closest_trials_to_times(., times = seq(0, 50 * 60, by = 60)) })

time_bin_means <- sampled_inventories %>%
  group_by(Strategy, SampledTime) %>%
  summarize(NumInnovations = mean(NumInnovations)) %>%
  ungroup() %>%
  recode_strategy() %>%
  recode_groups_by_generation()

performance_over_time_plot <- ggplot(TotemsTrials) +
  aes(TeamTime, NumInnovations, color = StrategyLabel) +
  geom_line(aes(SampledTime, group = GenerationStrategy), data = time_bin_means, alpha = 0.4) +
  scale_x_time("Team time", breaks = seconds(c(0, 25 * 60, 50 * 60))) +
  ylab("Number of inventions") +
  totems_theme["scale_color_strategy"] +
  totems_theme["base_theme"] +
  theme(legend.position = "top")
performance_over_time_plot
