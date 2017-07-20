source("docs/R/setup.R")

# ---- invention-times

InventionTimes <- Guesses %>%
  # Merge in TeamID and PlayerIX based on PlayerID
  left_join(PlayerInfo) %>%
  # Merge in TeamSize based on TeamID
  left_join(TeamInfo) %>%
  # Filter correct and unique guesses from 2-person synchronic teams
  filter(
    Strategy == "Synchronic",
    TeamSize == 2,
    Result != 0,
    UniqueItem == 1
  ) %>%
  # Arrange rows by Team and TeamTime
  arrange(TeamID, Result, TeamTime) %>%
  select(TeamID, InventionID = Result, PlayerIX, Time = TeamTime) %>%
  spread(PlayerIX, Time) %>%
  mutate(P1_minus_P2 = P1 - P2)

team_lags_plot <- ggplot(InventionTimes) +
  aes(P1_minus_P2) +
  geom_dotplot(binwidth=60) +
  facet_wrap("TeamID")

PerformanceByLag <- InventionTimes %>%
  group_by(TeamID) %>%
  summarize(Lag = mean(abs(P1_minus_P2), na.rm = TRUE)) %>%
  left_join(TeamPerformance, .)

performance_by_lag_plot <- ggplot(PerformanceByLag) +
  aes(Lag, NumInnovations) +
  geom_point() +
  geom_smooth(method = "lm")
