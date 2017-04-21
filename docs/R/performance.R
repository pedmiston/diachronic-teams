source("docs/R/setup.R")

# ---- performance
Performance <- TeamPerformance %>%
  recode_strategy()

innovations_mod <- lm(
  NumInnovations ~ Diachronic_v_Synchronic + Diachronic_v_Isolated,
  data = Performance
)

innovations_preds <- get_lm_mod_preds(innovations_mod) %>%
  rename(NumInnovations = fit, SE = se.fit) %>%
  recode_strategy()

innovations_plot <- ggplot(Performance) +
  aes(StrategyLabel, NumInnovations) +
  geom_point(aes(color = StrategyLabel),
             position = position_jitter(width = 0.3),
             alpha = 0.8) +
  geom_bar(aes(fill = StrategyLabel),
           stat = "summary", fun.y = "mean", alpha = 0.6) +
  geom_errorbar(aes(ymin = NumInnovations - SE, ymax = NumInnovations + SE),
                width = 0.2, data = innovations_preds) +
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
data("SampledPerformance")

# First step is to summarize inventories at particular time points
SampledPerformanceMeans <- SampledPerformance %>%
  left_join(PlayerInfo) %>%
  group_by(Strategy, SampledTime) %>%
  summarize(
    NumInnovations = mean(NumInnovations),
    Score = mean(Score)
  ) %>%
  ungroup() %>%
  recode_strategy() %>%
  guess_generation("SampledTime") %>%
  recode_groups_by_generation()

# Add in points at the origin (which couldn't be sampled)
trial0 <- data_frame(
  Strategy = c("Diachronic", "Diachronic", "Synchronic", "Isolated"),
  Generation = c(1, 2, 1, 1),
  SampledTime = c(0, 60*25, 0, 0),
  NumInnovations = 0,
  Score = 0
) %>%
  recode_strategy() %>%
  recode_groups_by_generation()

SampledPerformanceMeans %<>%
  bind_rows(trial0)

# Create the team time plot
innovations_over_time_plot <- ggplot(SampledPerformanceMeans) +
  aes(SampledTime, NumInnovations, color = StrategyLabel, group = GenerationStrategy) +
  geom_line(size = 1.2) +
  scale_x_time("Team time", breaks = seconds(c(0, 25 * 60, 50 * 60))) +
  scale_y_continuous("Number of inventions", breaks = seq(0, 20, by = 2)) +
  totems_theme["scale_color_strategy"] +
  totems_theme["base_theme"] +
  theme(legend.position = "top")

# Create the player time plot
SampledPerformancePlayerMeans <- SampledPerformance %>%
  left_join(PlayerInfo) %>%
  filter(!(Strategy == "Diachronic" & SampledPlayerTime > 25 * 60)) %>%
  group_by(Strategy, Generation, SampledPlayerTime) %>%
  summarize(NumInnovations = mean(NumInnovations)) %>%
  ungroup() %>%
  recode_strategy() %>%
  recode_groups_by_generation()

SampledPerformancePlayerMeans %<>%
  bind_rows(trial0) %>%
  filter(!(GenerationStrategy == "Diachronic-2" & SampledTime == 1500 & NumInnovations == 0))

diachronic_generation_labels <- SampledPerformancePlayerMeans %>%
  filter(Strategy == "Diachronic") %>%
  group_by(Strategy, Generation) %>%
  summarize(
    MaxInnovations = max(NumInnovations),
    SampledPlayerTime = max(SampledPlayerTime, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(Label = paste0("G", Generation)) %>%
  recode_strategy() %>%
  recode_groups_by_generation()
  
performance_stacked_plot <- ggplot(SampledPerformancePlayerMeans) +
  aes(SampledPlayerTime, y = NumInnovations, color = StrategyLabel) +
  geom_line(aes(group = GenerationStrategy), size = 1.2) +
  geom_text(aes(y = MaxInnovations, label = Label),
            data = diachronic_generation_labels,
            nudge_x = 2 * 60, show.legend = FALSE) +
  scale_x_time("Player time", breaks = seconds(c(0, 25 * 60, 50 * 60))) +
  scale_y_continuous("Number of inventions", breaks = seq(0, 20, by = 2)) +
  totems_theme["scale_color_strategy"] +
  totems_theme["base_theme"] +
  theme(legend.position = "top")

# ---- score
score_mod <- lm(
  Score ~ Diachronic_v_Synchronic + Diachronic_v_Isolated,
  data = Performance
)

score_preds <- get_lm_mod_preds(score_mod) %>%
  rename(Score = fit, SE = se.fit) %>%
  recode_strategy()

score_plot <- ggplot(Performance) +
  aes(StrategyLabel, Score) +
  geom_point(aes(color = StrategyLabel),
             position = position_jitter(width = 0.3),
             alpha = 0.8) +
  geom_bar(aes(fill = StrategyLabel),
           stat = "summary", fun.y = "mean", alpha = 0.6) +
  geom_errorbar(aes(ymin = Score - SE, ymax = Score + SE),
                width = 0.2, data = score_preds) +
  ylab("Totem score") +
  totems_theme["scale_x_strategy"] +
  totems_theme["scale_color_strategy"] +
  totems_theme["scale_fill_strategy"] +
  totems_theme["base_theme"] +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    axis.title.x = element_blank()
  )

score_over_time_plot <- ggplot(SampledPerformanceMeans) +
  aes(SampledTime, Score, color = StrategyLabel, group = GenerationStrategy) +
  geom_line(size = 1.2) +
  scale_x_time("Team time", breaks = seconds(c(0, 25 * 60, 50 * 60))) +
  # scale_y_continuous("Number of inventions", breaks = seq(0, 20, by = 2)) +
  totems_theme["scale_color_strategy"] +
  totems_theme["base_theme"] +
  theme(legend.position = "top")