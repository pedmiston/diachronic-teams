source("docs/R/setup.R")

# ---- redundancy
Redundancy <- Inventories %>%
  left_join(PlayerInfo) %>%
  recode_strategy() %>%
  mutate(
    TeamRedundancy = 1 - (TeamUniqueGuesses/TeamGuesses),
    PlayerRedundancy = 1 - (UniqueGuesses/TeamGuesses)
  )

team_redundancy_mod <- lmer(
  TeamRedundancy ~ Diachronic_v_Synchronic + Diachronic_v_Isolated + (1|TeamID),
  data = Redundancy
)

individual_redundancy_mod <- lmer(
  PlayerRedundancy ~ Diachronic_v_Synchronic + Diachronic_v_Isolated + (1|PlayerID),
  data = Redundancy
)

team_redundancy_preds <- recode_strategy() %>%
  cbind(., AICcmodavg::predictSE(team_redundancy_mod, newdata = ., se = TRUE)) %>%
  rename(Redundancy = fit, SE = se.fit)

individual_redundancy_preds <- recode_strategy() %>%
  cbind(., AICcmodavg::predictSE(individual_redundancy_mod, newdata = ., se = TRUE)) %>%
  rename(Redundancy = fit, SE = se.fit)

redundancy_measure_labels <- data_frame(
  RedundancyMeasure = c("TeamRedundancy", "PlayerRedundancy"),
  RedundancyLabel = c("Team redundancy", "Individual redundancy")
)

Redundancy <- Redundancy %>%
  gather(RedundancyMeasure, Redundancy, -(TeamID:Diachronic_v_Isolated)) %>%
  left_join(redundancy_measure_labels)

redundancy_preds <- bind_rows(
  TeamRedundancy = team_redundancy_preds,
  PlayerRedundancy = individual_redundancy_preds,
  .id = "RedundancyMeasure"
) %>%
  left_join(redundancy_measure_labels)

redundancy_plot <- ggplot(redundancy_preds) +
  aes(StrategyLabel, Redundancy) +
  geom_bar(aes(fill = StrategyLabel), stat = "summary", fun.y = "mean", alpha = 0.6) +
  geom_errorbar(aes(ymin = Redundancy - SE, ymax = Redundancy + SE),
                width = 0.2) +
  facet_wrap("RedundancyLabel", strip.position = "left") +
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
