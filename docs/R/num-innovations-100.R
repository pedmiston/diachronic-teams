source("docs/R/setup.R")

# ---- num-innovations-100

scale_x_calendar_time_100 <- scale_x_continuous(
  "Calendar time (min)",
  breaks = seq(0, 6000, by = 60 * 25),
  labels = seq(0, 100, by = 25))

scale_x_player_time_100 <- scale_x_continuous(
  "Learning time (min)",
  breaks = seq(0, 6000, by = 60 * 25),
  labels = seq(0, 100, by = 25))

scale_x_team_time_100 <- scale_x_continuous(
  "Labor time (min)",
  breaks = seq(0, 6000, by = 60 * 25),
  labels = seq(0, 100, by = 25))

# Final num innovations ----
data("PlayerPerformance")

PlayerPerformance100 <- PlayerPerformance %>%
  filter(
    TeamStatus == "V",
    Exp == "100LaborMinutes"
  ) %>%
  recode_strategy() %>%
  recode_session_type_100() %>%
  highlight_inheritance_100()

final_num_innovations_100_mod <- lm(
  NumInnovations ~
    DG4_v_DG1 + DG4_v_DG2 + DG4_v_DG3 +
    DG4_v_IS1 + DG4_v_IS2 + DG4_v_IS3 + DG4_v_IS4 +
    DG4_v_S4,
  data = PlayerPerformance100
)

final_num_innovations_100_preds <- recode_session_type_100() %>%
  cbind(., predict(final_num_innovations_100_mod, newdata = ., se = TRUE)) %>%
  rename(NumInnovations = fit, SE = se.fit) %>%
  recode_strategy() %>%
  highlight_inheritance_100()

set.seed(432)
final_num_innovations_100_plot <- ggplot(PlayerPerformance100) +
  aes(SessionTypeOrdered, NumInnovations) +
  geom_bar(aes(fill = StrategyLabel, alpha = Inheritance),
           stat = "identity", data = final_num_innovations_100_preds) +
  geom_point(aes(color = StrategyLabel),
             position = position_jitter(width = 0.3)) +
  geom_linerange(aes(ymin = NumInnovations-SE, ymax = NumInnovations+SE),
                 data = final_num_innovations_100_preds) +
  totems_theme$scale_color_strategy +
  totems_theme$scale_fill_strategy +
  scale_alpha_manual(values = c(0.7, 0.4)) +
  xlab("") +
  totems_theme$scale_y_num_innovations +
  scale_shape_manual(values = c(16, 1)) +
  totems_theme$base_theme +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  ) +
  ggtitle("Final number of innovations discovered by each strategy")

# Rate of innovation ----
data("Sampled")

Sampled100 <- Sampled %>%
  filter(
    TeamStatus == "V",
    Exp == "100LaborMinutes"
  ) %>%
  recode_strategy() %>%
  highlight_inheritance_100() %>%
  mutate(NumInnovations = InventorySize - 6)

num_innovations_over_time_100 <- ggplot(Sampled100) +
  aes(y = NumInnovations) +
  geom_line(aes(color = StrategyLabel,
                group = interaction(StrategyLabel, Generation),
                size = Inheritance),
            stat = "summary", fun.y = "mean") +
  totems_theme$scale_color_strategy +
  scale_size_manual(values = c(1.8, 1.0)) +
  totems_theme$scale_y_num_innovations +
  guides(size = "none") +
  totems_theme$base_theme

# Num innovations by generation ----

data("PlayerPerformance")

recode_generation_type_100 <- function(frame) {
  generation_type_levels <- c("GN", "GN_1", "IN", "IN_1")
  generation_type_labels <- c("Generation N", "Generation N+1", "Session N", "Session N+1")
  generation_type_map <- data_frame(
    Strategy = rep(c("Diachronic", "Isolated"), each = 6),
    Generation = rep(c(1, 2, 2, 3, 3, 4), 2),
    GenerationC = rep(c(-0.5, 0.5), times = 6),
    GenerationType = c(rep(c("GN", "GN_1"), 3), rep(c("IN", "IN_1"), 3)),
    GenerationTypeGroup = rep(rep(1:3, each = 2), 2),
    GenerationTypeLabel = factor(GenerationType, levels = generation_type_levels, labels = generation_type_labels)
  )
  if(missing(frame)) return(generation_type_map)
  left_join(frame, generation_type_map)
}

NumInnovationsByGeneration100 <- PlayerPerformance %>%
  filter(
    TeamStatus == "V",
    Exp == "100LaborMinutes",
    Strategy != "Synchronic"
  ) %>%
  recode_strategy() %>%
  recode_generation_type_100()

num_innovations_by_generation_100_mod <- lmer(
  NumInnovations ~ Diachronic_v_Isolated * GenerationC + (GenerationC|TeamID),
  data = NumInnovationsByGeneration100)

num_innovations_by_generation_100_preds <- expand.grid(
  GenerationC = c(-0.5, 0.5),
  Strategy = c("Diachronic", "Isolated"),
  stringsAsFactors = FALSE
) %>%
  mutate(GenerationTypeLabel = c("Generation N", "Generation N+1", "Session N", "Session N+1")) %>%
  recode_strategy() %>%
  cbind(., predictSE(num_innovations_by_generation_100_mod, newdata = ., se = TRUE)) %>%
  rename(NumInnovations = fit, SE = se.fit)

num_innovations_by_generation_100_plot <- ggplot(NumInnovationsByGeneration100) +
  aes(GenerationTypeLabel, NumInnovations, color = StrategyLabel) +
  geom_line(aes(group = interaction(TeamID, GenerationTypeGroup)), alpha = 0.4) +
  geom_line(aes(group = 1), size = 1.5,
            stat = "identity", data = num_innovations_by_generation_100_preds) +
  geom_errorbar(aes(ymin = NumInnovations-SE, ymax = NumInnovations+SE),
                data = num_innovations_by_generation_100_preds,
                width = 0.1, size = 1.5) +
  scale_x_discrete("") +
  totems_theme$scale_y_num_innovations +
  scale_color_manual(values = totems_theme$color_picker(c("orange", "blue"))) +
  facet_wrap("Strategy", scales = "free_x") +
  totems_theme$base_theme +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank())

# Unique innovations by generation ----
UniqueInnovations100 <- NumInnovationsByGeneration100 %>%
  group_by(TeamID, GenerationTypeGroup) %>%
  mutate(UniqueInnovations = ifelse(
      GenerationC == -0.5, NumInnovations[GenerationC == -0.5],
      NumInnovations[GenerationC == 0.5] - NumInnovations[GenerationC == -0.5]
    )
  ) %>%
  ungroup()

unique_innovations_100_mod <- lmer(
  UniqueInnovations ~ Diachronic_v_Isolated * GenerationC + (1|TeamID),
  data = UniqueInnovations100
)

unique_innovations_100_preds <- expand.grid(
  GenerationC = c(-0.5, 0.5),
  Strategy = c("Diachronic", "Isolated"),
  stringsAsFactors = FALSE
) %>%
  mutate(GenerationTypeLabel = c("Generation N", "Generation N+1", "Session N", "Session N+1")) %>%
  recode_strategy() %>%
  cbind(., predictSE(unique_innovations_100_mod, newdata = ., se = TRUE)) %>%
  rename(UniqueInnovations = fit, SE = se.fit)

num_unique_innovations_100_plot <- ggplot(UniqueInnovations100) +
  aes(GenerationTypeLabel, UniqueInnovations, color = StrategyLabel) +
  geom_line(aes(group = interaction(TeamID, GenerationTypeGroup)), alpha = 0.4) +
  geom_line(aes(group = 1), stat = "identity", size = 1.5,
            data = unique_innovations_100_preds) +
  geom_errorbar(aes(ymin = UniqueInnovations-SE, ymax = UniqueInnovations+SE),
                data = unique_innovations_100_preds,
                width = 0.1, size = 1.5) +
  scale_x_discrete("") +
  scale_y_continuous("Unique innovations", breaks = seq(-30, 30, by = 2)) +
  scale_color_manual(values = totems_theme$color_picker(c("orange", "blue"))) +
  facet_wrap("Strategy", scales = "free_x") +
  totems_theme$base_theme +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank())
