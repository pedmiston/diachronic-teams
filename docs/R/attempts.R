source("docs/R/setup.R")

# ---- attempts
individual_attempts_mod <- lm(
  NumGuesses ~ Diachronic_v_Synchronic + Diachronic_v_Isolated,
  data = TotemsPlayers
)

individual_attempts_preds <- get_lm_mod_preds(individual_attempts_mod) %>%
  rename(NumGuesses = fit, SE = se.fit)

team_attempts_mod <- lm(
  NumGuesses ~ Diachronic_v_Synchronic + Diachronic_v_Isolated,
  data = TotemsTeams
)

team_attempts_preds <- get_lm_mod_preds(team_attempts_mod) %>%
  rename(NumGuesses = fit, SE = se.fit)

Attempts <- bind_rows(
  `Team attempts` = TotemsTeams,
  `Individual attempts` = TotemsPlayers,
  .id = "AttemptMeasure"
)

AttemptsPreds <- bind_rows(
    `Team attempts` = team_attempts_preds,
    `Individual attempts` = individual_attempts_preds,
    .id = "AttemptMeasure"
  ) %>%
  recode_strategy()

attempts_plot <- ggplot(Attempts) +
  aes(StrategyLabel, NumGuesses) +
  geom_point(aes(color = StrategyLabel),
             position = position_jitter(width = 0.4)) +
  geom_bar(aes(fill = StrategyLabel), stat = "summary", fun.y = "mean",
           alpha = 0.6) +
  geom_errorbar(aes(ymin = NumGuesses - SE, ymax = NumGuesses + SE),
                data = AttemptsPreds, width = 0.2) +
  facet_wrap("AttemptMeasure", strip.position = "left") +
  totems_theme["scale_x_strategy"] +
  totems_theme["scale_color_strategy"] +
  totems_theme["scale_fill_strategy"] +
  totems_theme["base_theme"] +
  theme(
    legend.position = "none",
    strip.placement = "outside",
    axis.title.y = element_blank()
  )

performance_by_attempts_plot <- ggplot(Attempts) +
  aes(NumGuesses, NumInnovations, color = StrategyLabel) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap("AttemptMeasure", strip.position = "bottom") +
  scale_x_continuous("Team attempts") +
  scale_y_continuous("Number of inventions") +
  totems_theme["scale_color_strategy"] +
  totems_theme["base_theme"] +
  theme(
    legend.position = "top",
    strip.placement = "outside",
    axis.title.x = element_blank()
  )
