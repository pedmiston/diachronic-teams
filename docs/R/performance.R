
# ---- performance-100

# Performance over time
data("Sampled")

Sampled100 <- Sampled %>%
  recode_strategy() %>%
  mutate(NumInnovations = InventorySize - 6) %>%
  filter(
    TeamStatus == "V",
    Exp == "100LaborMinutes"
  )

scale_x_labor_time_100 <- scale_x_continuous(
  "Labor time (min)",
  breaks = seq(0, 6000, by = 60 * 5),
  labels = seq(0, 100, by = 5))

performance_over_time_100 <- ggplot(Sampled100) +
  aes(TeamTime, NumInnovations,
      group = interaction(Strategy, Generation),
      color = Strategy) +
  geom_line(stat = "summary", fun.y = "mean") +
  scale_x_labor_time_100 +
  scale_y_num_innovations

# Final performance
data("TeamPerformance")

TeamPerformance100 <- TeamPerformance %>%
  recode_strategy() %>%
  filter(
    TeamStatus == "V",
    Exp == "100LaborMinutes"
  )

set.seed(432)
final_performance_100 <- ggplot(TeamPerformance100) +
  aes(Strategy, NumInnovations) +
  geom_bar(aes(fill = Strategy), stat = "summary", fun.y = "mean", alpha = 0.4) +
  geom_point(aes(color = Strategy),
             position = position_jitter(width = 0.2, height = 0.2)) +
  scale_y_num_innovations +
  theme(legend.position = "none")
