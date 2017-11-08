source("docs/R/setup.R")

# ---- performance-50

# Performance over time
data("Sampled")

Sampled50 <- Sampled %>%
  recode_strategy() %>%
  mutate(NumInnovations = InventorySize - 6) %>%
  filter(
    TeamStatus == "V",
    Exp == "50LaborMinutes"
  )

scale_x_labor_time_50 <- scale_x_continuous(
  "Labor time (min)",
  breaks = seq(0, 3000, by = 60 * 5),
  labels = seq(0, 50, by = 5))

scale_y_num_innovations <- scale_y_continuous(
  "Number of innovations",
  breaks = seq(0, 30, by = 2))

performance_over_time_50 <- ggplot(Sampled50) +
  aes(TeamTime, NumInnovations,
      group = interaction(Strategy, SessionDuration, Generation),
      color = Strategy, linetype = factor(SessionDuration)) +
  geom_line(stat = "summary", fun.y = "mean") +
  scale_x_labor_time_50 +
  scale_y_num_innovations +
  theme(legend.position = c(0.15, 0.8))

# Final performance
data("TeamPerformance")

TeamPerformance50 <- TeamPerformance %>%
  filter(
    TeamStatus == "V",
    Exp == "50LaborMinutes"
  ) %>%
  recode_strategy()

set.seed(432)
final_performance_50 <- ggplot(TeamPerformance50) +
  aes(Strategy, NumInnovations, group = SessionDuration) +
  geom_bar(aes(fill = Strategy, group = SessionDuration),
           position = position_dodge(),
           stat = "summary", fun.y = "mean", alpha = 0.4) +
  geom_point(aes(color = Strategy, shape = factor(SessionDuration)),
             position = position_jitterdodge(jitter.height = 0.2,
                                             jitter.width = 0.4,
                                             dodge.width = 1.01)) +
  scale_y_num_innovations +
  scale_shape_manual(values = c(16, 1)) +
  theme(legend.position = "none")

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
