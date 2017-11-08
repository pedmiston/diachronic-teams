source("docs/R/setup.R")

# ---- performance-by-generation-50
data("PlayerPerformance")

PerformanceByGeneration50 <- PlayerPerformance %>%
  recode_strategy() %>%
  filter(
    TeamStatus == "V",
    Exp == "50LaborMinutes",
    Strategy != "Synchronic",
    SessionDuration == 25
  )

performance_by_generation_50_mod <- lmer(
  NumInnovations ~ Diachronic_v_Isolated * Generation + (1|TeamID),
  data = PerformanceByGeneration50
)

performance_by_generation_50_preds <- expand.grid(
  Generation = 1:2,
  Strategy = c("Diachronic", "Isolated"),
  stringsAsFactors = FALSE
) %>%
  recode_strategy() %>%
  cbind(., predictSE(performance_by_generation_50_mod, newdata = ., se = TRUE)) %>%
  rename(NumInnovations = fit, SE = se.fit)

performance_by_generation_50_plot <- ggplot(PerformanceByGeneration50) +
  aes(Generation, NumInnovations) +
  geom_line(aes(group = TeamID), alpha = 0.4) +
  geom_line(stat = "identity", size = 2,
            data = performance_by_generation_50_preds) +
  geom_errorbar(aes(ymin = NumInnovations-SE, ymax = NumInnovations+SE),
                data = performance_by_generation_50_preds,
                width = 0.2, size = 2) +
  scale_x_continuous("Generation", breaks = 1:2) +
  facet_wrap("Strategy")

# ---- performance-by-generation-100
data("PlayerPerformance")

PerformanceByGeneration100 <- PlayerPerformance %>%
  filter(
    TeamStatus == "V",
    Exp == "100LaborMinutes",
    Strategy != "Synchronic",
    SessionDuration == 25
  )

ggplot(PerformanceByGeneration100) +
  aes(Generation, NumInnovations) +
  geom_bar(stat = "summary", fun.y = "mean", alpha = 0.4) +
  geom_line(aes(group = TeamID), alpha = 0.4) +
  geom_line(stat = "summary", fun.y = "mean", size = 2) +
  facet_wrap("Strategy")