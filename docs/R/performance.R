source("docs/R/setup.R")

# ---- performance
data("TeamPerformance")

TeamPerformance %<>%
  filter(TeamStatus == "V") %>%
  recode_experiment()

set.seed(432)
ggplot(TeamPerformance) +
  aes(Strategy, NumInnovations, group = SessionDuration) +
  facet_wrap("ExpLabel") +
  geom_bar(aes(fill = Strategy, group = SessionDuration),
           position = position_dodge(),
           stat = "summary", fun.y = "mean", alpha = 0.4) +
  geom_point(aes(color = Strategy, shape = factor(SessionDuration)),
             position = position_jitterdodge(jitter.height = 0.2,
                                             jitter.width = 0.4,
                                             dodge.width = 1.01)) +
  scale_shape_manual(values = c(16, 1)) +
  theme(legend.position = "none")

# ---- likelihood-of-totem
TeamPerformance %<>%
  mutate(GotTotem = ifelse(NumInnovations < 10, 0, 1))

ggplot(TeamPerformance) +
  aes(Strategy, GotTotem, group = SessionDuration) +
  facet_wrap("ExpLabel") +
  geom_bar(aes(fill = Strategy, group = SessionDuration),
           position = position_dodge(),
           stat = "summary", fun.y = "mean", alpha = 0.4) +
  theme(legend.position = "none")

# ---- performance-by-generation
data("PlayerPerformance")

PerformanceByGeneration <- PlayerPerformance %>%
  filter(
    Strategy != "Synchronic",
    SessionDuration == 25,
    TeamStatus == "V",
    SessionStatus == "V"
  ) %>%
  recode_experiment()

ggplot(PerformanceByGeneration) +
  aes(Generation, NumInnovations) +
  geom_bar(stat = "summary", fun.y = "mean", alpha = 0.4) +
  geom_line(aes(group = TeamID), alpha = 0.4) +
  geom_point(aes(shape = factor(GotTotem))) +
  geom_line(stat = "summary", fun.y = "mean", size = 2) +
  facet_grid(ExpLabel ~ Strategy) +
  scale_shape

