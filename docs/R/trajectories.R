source("docs/R/setup.R")

# ---- trajectories
data("Trajectories")

Trajectories %<>%
  rename(Strategy = Treatment)

TrajectoryCounts <- Trajectories %>%
  group_by(Strategy) %>%
  summarize(NumUniqueTrajectories = length(unique(TrajectoryID))) %>%
  recode_strategy()

trajectory_count_plot <- ggplot(TrajectoryCounts) +
  aes(StrategyLabel, NumUniqueTrajectories) +
  geom_bar(aes(fill = StrategyLabel), stat = "identity") +
  ylab("Number of unique trajectories") +
  totems_theme["scale_x_strategy"] +
  totems_theme["scale_fill_strategy"] +
  totems_theme["base_theme"] +
  theme(legend.position = "none")
