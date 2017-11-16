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
