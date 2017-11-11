source("docs/R/setup.R")

# ---- num-innovations-50

scale_x_calendar_time_50 <- scale_x_continuous(
  "Calendar time (min)",
  breaks = seq(0, 3000, by = 60 * 5),
  labels = seq(0, 50, by = 5))

scale_x_player_time_50 <- scale_x_continuous(
  "Learning time (min)",
  breaks = seq(0, 3000, by = 60 * 5),
  labels = seq(0, 50, by = 5))

scale_x_team_time_50 <- scale_x_continuous(
  "Labor time (min)",
  breaks = seq(0, 3000, by = 60 * 5),
  labels = seq(0, 50, by = 5))

scale_y_num_innovations <- scale_y_continuous(
  "Number of innovations",
  breaks = seq(0, 30, by = 2))

# Final performance ----
data("PlayerPerformance")

PlayerPerformance50 <- PlayerPerformance %>%
  recode_strategy() %>%
  recode_session_type_50() %>%
  highlight_inheritance() %>%
  filter(
    TeamStatus == "V",
    Exp == "50LaborMinutes"
  )

final_num_innovations_50_mod <- lm(
  NumInnovations ~ DG2_v_DG1 + DG2_v_S2 + DG2_v_I50 + DG2_v_IS1 + DG2_v_IS2,
  data = PlayerPerformance50)

final_num_innovations_50_preds <- recode_session_type_50() %>%
  cbind(., predict(final_num_innovations_50_mod, newdata = ., se = TRUE)) %>%
  rename(NumInnovations = fit, SE = se.fit) %>%
  recode_strategy() %>%
  highlight_inheritance()

set.seed(432)
final_num_innovations_50_plot <- ggplot(PlayerPerformance50) +
  aes(SessionTypeOrdered, NumInnovations) +
  geom_bar(aes(fill = StrategyLabel, alpha = Inheritance),
           stat = "identity", data = final_num_innovations_50_preds) +
  geom_point(aes(color = StrategyLabel),
             position = position_jitter(width = 0.3)) +
  geom_linerange(aes(ymin = NumInnovations-SE, ymax = NumInnovations+SE),
                 data = final_num_innovations_50_preds) +
  totems_theme$scale_color_strategy +
  totems_theme$scale_fill_strategy +
  scale_alpha_manual(values = c(0.7, 0.4)) +
  xlab("") +
  scale_y_num_innovations +
  scale_shape_manual(values = c(16, 1)) +
  totems_theme$base_theme +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  ) +
  ggtitle("Final number of innovations discovered by each strategy")

# Performance over time ----
data("Sampled")

Sampled50 <- Sampled %>%
  recode_strategy() %>%
  highlight_inheritance() %>%
  mutate(NumInnovations = InventorySize - 6) %>%
  filter(
    TeamStatus == "V",
    Exp == "50LaborMinutes"
  )

num_innovations_over_time_50 <- ggplot(Sampled50) +
  aes(y = NumInnovations) +
  geom_line(aes(color = StrategyLabel,
                group = interaction(StrategyLabel, SessionDuration, Generation),
                linetype = factor(SessionDuration),
                size = Inheritance),
            stat = "summary", fun.y = "mean") +
  totems_theme$scale_color_strategy +
  scale_size_manual(values = c(1.8, 1.0)) +
  scale_y_num_innovations +
  guides(linetype = "none", size = "none") +
  totems_theme$base_theme

# Performance by generation ----
data("PlayerPerformance")

recode_generation_type <- function(frame) {
  generation_type_levels <- c("G1", "G2", "S1", "S2")
  generation_type_labels <- c("Generation 1", "Generation 2", "Session 1", "Session 2")
  generation_type_map <- data_frame(
    Strategy = c(rep("Diachronic", 2), rep("Isolated", 2)),
    Generation = rep(1:2, 2),
    GenerationType = generation_type_levels,
    GenerationTypeLabel = factor(generation_type_levels, levels = generation_type_levels, labels = generation_type_labels)
  )
  if(missing(frame)) return(generation_type_map)
  left_join(frame, generation_type_map)
}

NumInnovationsByGeneration50 <- PlayerPerformance %>%
  recode_strategy() %>%
  recode_generation_type() %>%
  filter(
    TeamStatus == "V",
    Exp == "50LaborMinutes",
    Strategy != "Synchronic",
    SessionDuration == 25
  )

num_innovations_by_generation_50_mod <- lmer(
  NumInnovations ~ Diachronic_v_Isolated * Generation + (1|TeamID),
  data = NumInnovationsByGeneration50
)

num_innovations_by_generation_50_preds <- expand.grid(
  Generation = 1:2,
  Strategy = c("Diachronic", "Isolated"),
  stringsAsFactors = FALSE
) %>%
  recode_strategy() %>%
  recode_generation_type() %>%
  cbind(., predictSE(num_innovations_by_generation_50_mod, newdata = ., se = TRUE)) %>%
  rename(NumInnovations = fit, SE = se.fit)

num_innovations_by_generation_50_plot <- ggplot(NumInnovationsByGeneration50) +
  aes(GenerationTypeLabel, NumInnovations, color = StrategyLabel) +
  geom_line(aes(group = TeamID), alpha = 0.4) +
  geom_line(aes(group = 1), stat = "identity", size = 1.5,
            data = num_innovations_by_generation_50_preds) +
  geom_errorbar(aes(ymin = NumInnovations-SE, ymax = NumInnovations+SE),
                data = num_innovations_by_generation_50_preds,
                width = 0.1, size = 1.5) +
  scale_x_discrete("") +
  scale_y_num_innovations +
  scale_color_manual(values = totems_theme$color_picker(c("orange", "blue"))) +
  facet_wrap("Strategy", scales = "free_x") +
  totems_theme$base_theme +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank())

# Additional innovations by generation ----
AdditionalInnovations50 <- NumInnovationsByGeneration50 %>%
  group_by(TeamID) %>%
  mutate(AdditionalInnovations = ifelse(
      Generation == 1, NumInnovations,
      NumInnovations[Generation == 2] - NumInnovations[Generation == 1]
    )
  ) %>%
  ungroup() %>%
  recode_generation_type()

additional_innovations_50_mod <- lmer(
  AdditionalInnovations ~ Diachronic_v_Isolated * Generation + (1|TeamID),
  data = AdditionalInnovations50
)

additional_innovations_50_preds <- expand.grid(
  Strategy = c("Diachronic", "Isolated"),
  Generation = 1:2,
  stringsAsFactors = FALSE
) %>%
  recode_strategy() %>%
  cbind(., predictSE(additional_innovations_50_mod, newdata = ., se = TRUE)) %>%
  rename(AdditionalInnovations = fit, SE = se.fit) %>%
  recode_generation_type()

num_additional_innovations_50_plot <- ggplot(AdditionalInnovations50) +
  aes(GenerationTypeLabel, AdditionalInnovations, color = StrategyLabel) +
  geom_line(aes(group = TeamID), alpha = 0.4) +
  geom_line(aes(group = 1), stat = "identity", size = 1.5,
            data = additional_innovations_50_preds) +
  geom_errorbar(aes(ymin = AdditionalInnovations-SE, ymax = AdditionalInnovations+SE),
                data = additional_innovations_50_preds,
                width = 0.1, size = 1.5) +
  scale_x_discrete("") +
  scale_y_continuous("Additional innovations", breaks = seq(0, 30, by = 2)) +
  scale_color_manual(values = totems_theme$color_picker(c("orange", "blue"))) +
  facet_wrap("Strategy", scales = "free_x") +
  totems_theme$base_theme +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank())
