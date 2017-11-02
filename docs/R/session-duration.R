source("docs/R/setup.R")

# ---- session-duration
data("TeamPerformance")

Isolated50Minutes <- TeamPerformance %<>%
  filter(
    Strategy == "Isolated",
    TeamStatus == "V",
    Exp == "50LaborMinutes"
  ) %>%
  recode_session_duration()

session_duration_mod <- lm(NumInnovations ~ SessionDurationLabel,
                           data = Isolated50Minutes)

session_duration_preds <- recode_session_duration() %>%
  cbind(., predict(session_duration_mod, ., se = TRUE)) %>%
  rename(NumInnovations = fit, SE = se.fit)

set.seed(210)
ggplot(Isolated50Minutes) +
  aes(SessionDurationLabel, NumInnovations) +
  geom_bar(stat = "summary", fun.y = "mean") +
  geom_errorbar(aes(ymin = NumInnovations-SE, ymax = NumInnovations+SE),
                data = session_duration_preds, width = 0.2) +
  geom_point(position = position_jitter(width = 0.05),
             size = 1) +
  ggtitle("Impact of session duration on isolated participants")
