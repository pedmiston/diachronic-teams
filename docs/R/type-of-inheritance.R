devtools::load_all(data)
source("docs/R/setup.R")
# ---- type-of-inheritance
data("PlayerPerformance")

NumInnovationsByGeneration100 <- PlayerPerformance %>%
  filter(
    TeamStatus == "V",
    Exp == "100LaborMinutes",
    Strategy != "Synchronic"
  ) %>%
  recode_strategy() %>%
  label_inheritance() %>%
  recode_inheritance() %>%
  recode_generation_type_100()

ggplot(NumInnovationsByGeneration100) +
  aes(Generation, NumInnovations, color = StrategyLabel) +
  geom_line(aes(group = TeamID), size = 0.4, alpha = 0.6) +
  geom_line(stat = "summary", fun.y = "mean", size = 1.4) +
  t_$scale_y_num_innovations +
  t_$scale_color_strategy +
  t_$base_theme

num_innovations_by_generation_100_mod <- lmer(
  NumInnovations ~ Diachronic_v_Isolated * GenerationC + (1|TeamID),
  data = NumInnovationsByGeneration100
)

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
  t_$scale_y_num_innovations +
  scale_color_manual(values = t_$color_picker(c("orange", "blue"))) +
  facet_wrap("Strategy", scales = "free_x") +
  t_$base_theme +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank())

num_innovations_by_generation_100_plot_grouped <- ggplot(NumInnovationsByGeneration100) +
  aes(GenerationTypeLabel, NumInnovations, color = StrategyLabel) +
  # geom_line(aes(group = interaction(TeamID, GenerationTypeGroup)), alpha = 0.4) +
  geom_line(aes(group = interaction(StrategyLabel, GenerationTypeGroup)), stat = "summary", fun.y = "mean")

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
  scale_color_manual(values = t_$color_picker(c("orange", "blue"))) +
  facet_wrap("Strategy", scales = "free_x") +
  t_$base_theme +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank())
