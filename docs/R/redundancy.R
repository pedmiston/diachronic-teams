source("docs/R/setup.R")

# ---- redundancy
team_redundancy_mod <- lmer(
  Redundancy ~ Diachronic_v_Synchronic + Diachronic_v_Isolated + (1|TeamID),
  data = TeamProblems
)

individual_redundancy_mod <- lmer(
  Redundancy ~ Diachronic_v_Synchronic + Diachronic_v_Isolated + (1|PlayerID),
  data = PlayerProblems
)

team_redundancy_preds <- recode_strategy() %>%
  cbind(., AICcmodavg::predictSE(team_redundancy_mod, newdata = ., se = TRUE)) %>%
  rename(Redundancy = fit, SE = se.fit)

individual_redundancy_preds <- recode_strategy() %>%
  cbind(., AICcmodavg::predictSE(individual_redundancy_mod, newdata = ., se = TRUE)) %>%
  rename(Redundancy = fit, SE = se.fit)

Redundancy <- bind_rows(
  `Team redundancy` = TeamProblems,
  `Individual redundancy` = PlayerProblems,
  .id = "RedundancyMeasure"
)

redundancy_preds <- bind_rows(
  `Team redundancy` = team_redundancy_preds,
  `Individual redundancy` = individual_redundancy_preds,
  .id = "RedundancyMeasure"
)

redundancy_plot <- ggplot(redundancy_preds) +
  aes(StrategyLabel, Redundancy) +
  geom_bar(aes(fill = StrategyLabel), stat = "summary", fun.y = "mean", alpha = 0.6) +
  geom_errorbar(aes(ymin = Redundancy - SE, ymax = Redundancy + SE),
                width = 0.2) +
  facet_wrap("RedundancyMeasure", strip.position = "left") +
  coord_cartesian(ylim = c(0, 0.6)) +
  totems_theme["scale_x_strategy"] +
  scale_y_continuous("Redundancy", labels = scales::percent) +
  totems_theme["scale_fill_strategy"] +
  totems_theme["base_theme"] +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    strip.placement = "outside",
    axis.title.y = element_blank()
  )
redundancy_plot
