source("docs/R/setup.R")

# ---- likelihood-50
data("TeamPerformance")

Likelihood50 <- TeamPerformance %>%
  recode_strategy() %>%
  mutate(GotTotem = as.numeric(NumInnovations > 10)) %>%
  filter(
    TeamStatus == "V",
    Exp == "50LaborMinutes"
  )

likelihood50_mod <- glm(
  GotTotem ~ (Diachronic_v_Isolated + Diachronic_v_Synchronic) + SessionDuration,
  data = Likelihood50, family = "binomial"
)

likelihood50_preds <- data_frame(
    SessionDuration = c(25, 25, 25, 50),
    Strategy = c("Diachronic", "Synchronic", "Isolated", "Isolated")
  ) %>%
  recode_strategy() %>%
  cbind(., predict(likelihood50_mod, newdata = ., se = TRUE, type = "response")) %>%
  rename(GotTotem = fit, SE = se.fit)

set.seed(432)
likelihood50_plot <- ggplot(Likelihood50) +
  aes(Strategy, GotTotem, group = SessionDuration) +
  geom_bar(aes(fill = Strategy, group = SessionDuration),
           position = position_dodge(width = 0.95),
           stat = "identity",
           data = likelihood50_preds) +
  geom_errorbar(aes(ymin = GotTotem-SE, ymax = GotTotem+SE),
                position = position_dodge(width = 0.95), width = 0.2,
                data = likelihood50_preds) +
  scale_y_continuous("Likelihood of creating a totem", labels = scales::percent) +
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
