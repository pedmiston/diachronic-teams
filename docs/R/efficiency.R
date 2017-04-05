source("docs/R/setup.R")

# ---- efficiency
player_generations <- TotemsPlayers %>%
  select(PlayerID, Generation)

remove_first_gen_diachronic <- . %>%
  left_join(player_generations) %>%
  filter(!(Strategy == "Diachronic" & Generation == 1))

team_efficiency_mod <- lmer(
  Guesses ~
    Diachronic_v_Synchronic + Diachronic_v_Isolated +
    (Diachronic_v_Synchronic + Diachronic_v_Isolated|TeamInventory) +
    NumTeamInnovations +
    (1|TeamID),
  data = TeamInventoryGuesses
)

individual_efficiency_mod <- lmer(
  Guesses ~
    Diachronic_v_Synchronic + Diachronic_v_Isolated +
    (Diachronic_v_Synchronic + Diachronic_v_Isolated|TeamInventory) +
    NumInnovations +
    (1|PlayerID),
  data = IndividualInventoryGuesses #%>% remove_first_gen_diachronic()
)

team_efficiency_preds <- expand.grid(
  Strategy = recode_strategy()$Strategy,
  NumTeamInnovations = mean(TeamInventoryGuesses$NumTeamInnovations),
  stringsAsFactors = FALSE
) %>%
  recode_strategy() %>%
  cbind(., AICcmodavg::predictSE(team_efficiency_mod, newdata = ., se = TRUE)) %>%
  rename(Guesses = fit, SE = se.fit)

individual_efficiency_preds <- expand.grid(
  Strategy = recode_strategy()$Strategy,
  NumInnovations = mean(IndividualInventoryGuesses$NumInnovations),
  stringsAsFactors = FALSE
) %>%
  recode_strategy() %>%
  cbind(., AICcmodavg::predictSE(individual_efficiency_mod, newdata = ., se = TRUE)) %>%
  rename(Guesses = fit, SE = se.fit)

efficiency_preds <- bind_rows(
  `Individual guesses per invention` = individual_efficiency_preds,
  `Team guesses per invention` = team_efficiency_preds,
  .id = "GuessMeasure"
)

efficiency_plot <- ggplot(efficiency_preds) +
  aes(StrategyLabel, Guesses) +
  geom_bar(aes(fill = StrategyLabel), stat = "identity",
           alpha = 0.6) +
  geom_errorbar(aes(ymin = Guesses - SE, ymax = Guesses + SE),
                width = 0.3) +
  facet_wrap("GuessMeasure", strip.position = "left") +
  totems_theme["scale_x_strategy"] +
  ylab("Guesses per invention") +
  totems_theme["scale_fill_strategy"] +
  totems_theme["base_theme"] +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    strip.placement = "outside",
    axis.title.y = element_blank()
  )
efficiency_plot
