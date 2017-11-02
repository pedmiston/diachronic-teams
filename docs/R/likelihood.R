# ---- likelihood-50
data("TeamPerformance")

TeamPerformance50 <- TeamPerformance %>%
  filter(
    TeamStatus == "V",
    Exp == "50LaborMinutes"
  ) %>%
  mutate(GotTotem = as.numeric(NumInnovations > 10))

set.seed(432)
ggplot(TeamPerformance50) +
  aes(Strategy, GotTotem, group = SessionDuration) +
  geom_bar(aes(fill = Strategy, group = SessionDuration),
           position = position_dodge(width = 0.95),
           stat = "summary", fun.y = "mean") +
  scale_shape_manual(values = c(16, 1)) +
  theme(legend.position = "none")

# ---- likelihood-100
data("TeamPerformance")

TeamPerformance100 <- TeamPerformance %>%
  filter(
    TeamStatus == "V",
    Exp == "100LaborMinutes"
  ) %>%
  mutate(GotTotem = as.numeric(NumInnovations > 10)) %>%
  recode_strategy()

likelihood100_mod <- glm(GotTotem ~ DvS + DSvI, data = TeamPerformance100)
likelihood100_preds <- recode_strategy() %>%
  cbind(., predict(likelihood100_mod, ., se = TRUE)) %>%
  rename(GotTotem = fit, SE = se.fit)

set.seed(432)
ggplot(TeamPerformance100) +
  aes(Strategy, GotTotem) +
  geom_bar(aes(fill = Strategy),
           stat = "summary", fun.y = "mean") +
  geom_errorbar(aes(ymin = GotTotem-SE, ymax = GotTotem+SE),
                data = likelihood100_preds, width = 0.2) +
  scale_shape_manual(values = c(16, 1)) +
  theme(legend.position = "none")
