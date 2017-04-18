source("docs/R/setup.R")

# ---- efficiency
PlayerEfficiency <- PlayerProblems %>%
  group_by(PlayerID, TeamID) %>%
  summarize(Guesses = mean(Guesses)) %>%
  ungroup() %>%
  left_join(TeamPerformance %>% select(TeamID, Strategy)) %>%
  recode_strategy() %>%
  left_join(PlayerTrials %>% select(PlayerID, Generation) %>% unique()) %>%
  recode_groups_by_generation()

generation_strategy_position <- position_jitterdodge(jitter.width = 0.4,
                                                     dodge.width = 0.41)

diachronic_generation_labels <- data_frame(
  Strategy = c("Diachronic", "Diachronic"),
  Generation = 1:2,
  GenerationLabel = paste0("G", Generation)
) %>%
  recode_strategy() %>%
  recode_groups_by_generation()

set.seed(342)
efficiency_plot <- ggplot(PlayerEfficiency) +
  aes(StrategyLabel, Guesses, shape = factor(Generation), color = StrategyLabel) +
  geom_point(position = generation_strategy_position) +
  geom_text(aes(label = GenerationLabel, y = 4),
            position = position_dodge(width = 0.4),
            data = diachronic_generation_labels) +
  totems_theme["scale_x_strategy"] +
  scale_y_reverse("Guesses per invention", breaks = c(1, seq(50, 250, by = 50))) +
  totems_theme["scale_color_strategy"] +
  scale_shape_manual("", values = c(16, 1)) +
  totems_theme["scale_fill_strategy"] +
  totems_theme["base_theme"] +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )
