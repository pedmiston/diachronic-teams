source("docs/R/setup.R")

# ---- num-innovations-50

# Final num innovations ----
data("PlayerPerformance")

PlayerPerformance50min <- PlayerPerformance %>%
  filter(TeamStatus == "V", Exp == "50LaborMinutes") %>%
  recode_strategy() %>%
  recode_session_type_50min()

final_num_innovations_50_mod <- lm(
  NumInnovations ~ DG2_v_DG1 + DG2_v_S2 + DG2_v_I50,
  data = PlayerPerformance50min)

final_num_innovations_50_preds <- recode_session_type_50min() %>%
  cbind(., predict(final_num_innovations_50_mod, newdata = ., se = TRUE)) %>%
  rename(NumInnovations = fit, SE = se.fit) %>%
  recode_strategy() %>%
  label_inheritance() %>%
  recode_inheritance()

set.seed(432)
final_num_innovations_50_plot <- ggplot(PlayerPerformance50min) +
  aes(SessionTypeOrdered, NumInnovations) +
  geom_bar(aes(fill = StrategyLabel, alpha = Inheritance),
           stat = "identity", data = final_num_innovations_50_preds) +
  geom_point(aes(color = StrategyLabel),
             position = position_jitter(width = 0.3)) +
  geom_linerange(aes(ymin = NumInnovations-SE, ymax = NumInnovations+SE),
                 data = final_num_innovations_50_preds) +
  t_$scale_color_strategy +
  t_$scale_fill_strategy +
  scale_alpha_manual(values = c(0.7, 0.4)) +
  xlab("") +
  t_$scale_y_num_innovations +
  scale_shape_manual(values = c(16, 1)) +
  t_$base_theme +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  ) +
  ggtitle("Final number of innovations discovered by each strategy")

# Rate of innovation ----
data("Sampled")

Sampled50 <- Sampled %>%
  filter(
    TeamStatus == "V",
    Exp == "50LaborMinutes"
  ) %>%
  recode_strategy() %>%
  label_inheritance() %>%
  recode_inheritance() %>%
  mutate(NumInnovations = InventorySize - 6)

num_innovations_over_time_50 <- ggplot(Sampled50) +
  aes(TeamTime, NumInnovations) +
  geom_line(aes(color = StrategyLabel,
                group = interaction(StrategyLabel, Generation),
                size = Inheritance),
            stat = "summary", fun.y = "mean") +
  t_$scale_x_team_time_50 +
  t_$scale_color_strategy +
  scale_size_manual(values = c(1.8, 1.0)) +
  t_$scale_y_num_innovations +
  guides(linetype = "none", size = "none") +
  t_$base_theme
