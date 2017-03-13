source("docs/R/setup.R")

# ---- trajectories
data("Trajectories")

Trajectories %<>%
  rename(Strategy = Treatment)

TrajectoryCounts <- Trajectories %>%
  group_by(Strategy) %>%
  summarize(
    NumUniqueTrajectories = length(unique(TrajectoryID)),
    NumTeams = length(unique(TeamID)),
    UniqueTrajectoriesPerTeam = NumUniqueTrajectories/NumTeams
  ) %>%
  recode_strategy()

trajectory_count_plot <- ggplot(TrajectoryCounts) +
  aes(StrategyLabel, UniqueTrajectoriesPerTeam) +
  geom_bar(aes(fill = StrategyLabel), stat = "identity", alpha = 0.6) +
  ylab("Unique trajectories discovered per team") +
  totems_theme["scale_x_strategy"] +
  totems_theme["scale_fill_strategy"] +
  totems_theme["base_theme"] +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )
        
