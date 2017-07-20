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
  filter(!(is.na(P1) | is.na(P2))) %>%
  mutate(
    P1_minus_P2 = P1 - P2,
    FirstPlayer = ifelse(P1 > P2, "P1", "P2")
  )

team_balance_plot <- ggplot(InventionTimes) +
  aes(FirstPlayer) +
  geom_histogram(stat = "count") +
  facet_wrap("TeamID")

PerformanceByBalance <- InventionTimes %>%
  group_by(TeamID) %>%
  summarize(Balance = sum(FirstPlayer == "P1")/n()) %>%
  left_join(TeamPerformance, .)

performance_by_balance_mod <- lm(NumInnovations ~ Balance,
                                 data = PerformanceByBalance)

performance_by_balance_preds <- data_frame(Balance = seq(0, 1, by = 0.05)) %>%
  cbind(., predict(performance_by_balance_mod, newdata = ., se = TRUE)) %>%
  rename(NumInnovations = fit, SE = se.fit)

performance_by_balance_plot <- ggplot(PerformanceByBalance) +
  aes(Balance, NumInnovations) +
  geom_point() +
  geom_smooth(aes(ymin = NumInnovations - SE, ymax = NumInnovations + SE),
              data = performance_by_balance_preds, stat = "identity")
