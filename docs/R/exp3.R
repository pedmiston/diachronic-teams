source("docs/R/_setup.R")

# ---- exp3

# Number of innovations ----
data("PlayerPerformance")

PlayerPerformance100min <- PlayerPerformance %>%
  filter(TeamStatus == "V", Exp == "100LaborMinutes") %>%
  recode_strategy() %>%
  recode_session_type_100() %>%
  label_inheritance()

num_innovations_50min_mod <- lmer(
  NumInnovations ~ DG4_v_DG1 + DG4_v_DG2 + DG4_v_DG3 + DG4_v_IS1 + DG4_v_IS2 + DG4_v_IS3 + DG4_v_IS4 + DG4_v_S4 + (1|TeamID),
  data = PlayerPerformance100min
)

# Use lm mod for error on plot because lmer mod preds with AICcmodavg look too small
num_innovations_100min_lm_mod <- lm(
  NumInnovations ~ DG4_v_DG1 + DG4_v_DG2 + DG4_v_DG3 + DG4_v_IS1 + DG4_v_IS2 + DG4_v_IS3 + DG4_v_IS4 + DG4_v_S4,
  data = PlayerPerformance100min
)

num_innovations_100min_preds <- recode_session_type_100() %>%
  cbind(., predict(num_innovations_100min_lm_mod, newdata = ., se = TRUE)) %>%
  rename(NumInnovations = fit, SE = se.fit) %>%
  recode_strategy() %>%
  recode_session_type_100() %>%
  highlight_inheritance_100()

set.seed(432)
num_innovations_100min_plot <- ggplot(PlayerPerformance100min) +
  aes(SessionTypeOrdered, NumInnovations) +
  geom_bar(aes(fill = StrategyLabel, alpha = Inheritance),
           stat = "identity", data = num_innovations_100min_preds) +
  geom_point(aes(color = StrategyLabel),
             position = position_jitter(width = 0.3)) +
  geom_linerange(aes(ymin = NumInnovations-SE, ymax = NumInnovations+SE),
                 data = num_innovations_100min_preds) +
  t_$scale_color_strategy +
  t_$scale_fill_strategy +
  scale_alpha_manual(values = c(0.7, 0.4, 0.4)) +
  xlab("") +
  t_$scale_y_num_innovations +
  scale_shape_manual(values = c(16, 1)) +
  t_$base_theme +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  ) +
  ggtitle("Number of innovations discovered using each strategy")

# Rate of innovation ----
data("Sampled")

Sampled50min <- Sampled %>%
  filter(
    TeamStatus == "V",
    Exp == "50LaborMinutes"
  ) %>%
  recode_strategy() %>%
  label_inheritance() %>%
  mutate(NumInnovations = InventorySize - 6)

innovation_rate_50min_plot <- ggplot(Sampled50min) +
  aes(TeamTime, NumInnovations) +
  geom_line(aes(color = StrategyLabel,
                group = interaction(StrategyLabel, Generation),
                size = Inheritance),
            stat = "summary", fun.y = "mean") +
  t_$scale_x_team_time +
  t_$scale_color_strategy +
  scale_size_manual(values = c(1.8, 1.0)) +
  t_$scale_y_num_innovations +
  guides(linetype = "none", size = "none") +
  t_$base_theme +
  theme(legend.position = c(0.2, 0.8))
