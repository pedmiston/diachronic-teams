source("docs/R/setup.R")

# ---- performance
inventory_mod <- lm(
  NumInnovations ~ Diachronic_v_Synchronic + Diachronic_v_Isolated,
  data = TeamPerformance
)

inventory_preds <- get_lm_mod_preds(inventory_mod) %>%
  rename(NumInnovations = fit, SE = se.fit) %>%
  recode_strategy()

performance_plot <- ggplot(TeamPerformance) +
  aes(StrategyLabel, NumInnovations) +
  geom_point(aes(color = StrategyLabel),
             position = position_jitter(width = 0.3),
             alpha = 0.8) +
  geom_bar(aes(fill = StrategyLabel),
           stat = "summary", fun.y = "mean", alpha = 0.6) +
  geom_errorbar(aes(ymin = NumInnovations - SE, ymax = NumInnovations + SE),
                width = 0.2, data = inventory_preds) +
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

# Create performance over time plots -------------------------------------------

# First step is to summarize inventories at particular time points
# for both TeamTrials and PlayerTrials.
SampledTeamTrialsMeans <- SampledTeamTrials %>%
  group_by(Strategy, SampledTime) %>%
  summarize(NumInnovations = mean(NumInnovations)) %>%
  ungroup() %>%
  recode_strategy() %>%
  guess_generation("SampledTime") %>%
  recode_groups_by_generation()

SampledPlayerTrialsMeans <- SampledPlayerTrials %>%
  group_by(Strategy, Generation, SampledTime) %>%
  summarize(NumInnovations = mean(NumInnovations)) %>%
  ungroup() %>%
  recode_strategy() %>%
  recode_groups_by_generation()

# Add in points at the origin (which couldn't be sampled)
trial0 <- data_frame(
  Strategy = c("Diachronic", "Diachronic", "Synchronic", "Isolated"),
  Generation = c(1, 2, 1, 1),
  SampledTime = c(0, 60*25, 0, 0),
  NumInnovations = 0
) %>%
  recode_strategy() %>%
  recode_groups_by_generation()

SampledTeamTrialsMeans %<>%
  bind_rows(trial0)

SampledPlayerTrialsMeans %<>%
  bind_rows(trial0) %>%
  filter(!(GenerationStrategy == "Diachronic-2" & SampledTime == 1500 & NumInnovations == 0))

# Create the performance over time plot
performance_over_time_plot <- ggplot(SampledTeamTrialsMeans) +
  aes(SampledTime, NumInnovations, color = StrategyLabel, group = GenerationStrategy) +
  geom_line(size = 1.2) +
  scale_x_time("Team time", breaks = seconds(c(0, 25 * 60, 50 * 60))) +
  scale_y_continuous("Number of inventions", breaks = seq(0, 20, by = 2)) +
  totems_theme["scale_color_strategy"] +
  totems_theme["base_theme"] +
  theme(legend.position = "top")

diachronic_generation_labels <- SampledPlayerTrialsMeans %>%
  filter(Strategy == "Diachronic") %>%
  group_by(Strategy, Generation) %>%
  summarize(
    MaxInnovations = max(NumInnovations),
    SampledTime = max(SampledTime, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(Label = paste0("G", Generation)) %>%
  recode_strategy() %>%
  recode_groups_by_generation()
  
performance_stacked_plot <- ggplot(SampledPlayerTrialsMeans) +
  aes(SampledTime, y = NumInnovations, color = StrategyLabel) +
  geom_line(aes(group = GenerationStrategy), size = 1.2) +
  geom_text(aes(y = MaxInnovations, label = Label),
            data = diachronic_generation_labels,
            nudge_x = 2 * 60, show.legend = FALSE) +
  scale_x_time("Player time", breaks = seconds(c(0, 25 * 60, 50 * 60))) +
  scale_y_continuous("Number of inventions", breaks = seq(0, 20, by = 2)) +
  totems_theme["scale_color_strategy"] +
  totems_theme["base_theme"] +
  theme(legend.position = "top")

SampledPlayerTrialsMeansDuplicated <- SampledPlayerTrialsMeans %>%
  filter(GenerationStrategy == "Diachronic-2") %>%
  mutate(
    SampledTime = SampledTime + 25 * 60,
    GenerationStrategy = "Diachronic-2a"
  ) %>%
  bind_rows(
    trial0 %>%
      filter(GenerationStrategy == "Diachronic-2") %>%
      mutate(GenerationStrategy = "Diachronic-2a")
  ) %>%
  bind_rows(SampledPlayerTrialsMeans)

diachronic_generation_labels %<>%
  filter(Generation == 2) %>%
  mutate(SampledTime = SampledTime + 25 * 60) %>%
  bind_rows(diachronic_generation_labels)

performance_duplicated_plot <- ggplot(SampledPlayerTrialsMeansDuplicated) +
  aes(SampledTime, y = NumInnovations, color = StrategyLabel) +
  geom_line(aes(group = GenerationStrategy), size = 1.2) +
  geom_text(aes(y = MaxInnovations, label = Label),
            data = diachronic_generation_labels,
            nudge_x = 2 * 60, show.legend = FALSE) +
  scale_x_time("Time", breaks = seconds(c(0, 25 * 60, 50 * 60))) +
  scale_y_continuous("Number of inventions", breaks = seq(0, 20, by = 2)) +
  guides(linetype = "none") +
  totems_theme["scale_color_strategy"] +
  totems_theme["base_theme"] +
  theme(legend.position = "top")
