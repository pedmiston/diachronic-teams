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

# ---- performance-50
data("TeamPerformance")

TeamPerformance50 <- TeamPerformance %>%
  filter(
    TeamStatus == "V",
    Exp == "50LaborMinutes"
  )

set.seed(432)
ggplot(TeamPerformance50) +
  aes(Strategy, NumInnovations, group = SessionDuration) +
  geom_bar(aes(fill = Strategy, group = SessionDuration),
           position = position_dodge(),
           stat = "summary", fun.y = "mean", alpha = 0.4) +
  geom_point(aes(color = Strategy, shape = factor(SessionDuration)),
             position = position_jitterdodge(jitter.height = 0.2,
                                             jitter.width = 0.4,
                                             dodge.width = 1.01)) +
  scale_shape_manual(values = c(16, 1)) +
  theme(legend.position = "none")

# ---- performance-100
data("TeamPerformance")

TeamPerformance100 <- TeamPerformance %>%
  filter(
    TeamStatus == "V",
    Exp == "100LaborMinutes"
  )

set.seed(432)
ggplot(TeamPerformance100) +
  aes(Strategy, NumInnovations) +
  geom_bar(aes(fill = Strategy), stat = "summary", fun.y = "mean", alpha = 0.4) +
  geom_point(aes(color = Strategy),
             position = position_jitter(width = 0.2, height = 0.2)) +
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
  geom_line(stat = "summary", fun.y = "mean", size = 2) +
  facet_grid(ExpLabel ~ Strategy)

# ---- performance-by-generation-50
data("PlayerPerformance")

PerformanceByGeneration50 <- PlayerPerformance %>%
  filter(
    TeamStatus == "V",
    Exp == "50LaborMinutes",
    Strategy != "Synchronic",
    SessionDuration == 25
  )

ggplot(PerformanceByGeneration50) +
  aes(Generation, NumInnovations) +
  geom_bar(stat = "summary", fun.y = "mean", alpha = 0.4) +
  geom_line(aes(group = TeamID), alpha = 0.4) +
  geom_line(stat = "summary", fun.y = "mean", size = 2) +
  facet_wrap("Strategy")

# ---- performance-by-generation-100
data("PlayerPerformance")

PerformanceByGeneration100 <- PlayerPerformance %>%
  filter(
    TeamStatus == "V",
    Exp == "100LaborMinutes",
    Strategy != "Synchronic",
    SessionDuration == 25
  )

ggplot(PerformanceByGeneration100) +
  aes(Generation, NumInnovations) +
  geom_bar(stat = "summary", fun.y = "mean", alpha = 0.4) +
  geom_line(aes(group = TeamID), alpha = 0.4) +
  geom_line(stat = "summary", fun.y = "mean", size = 2) +
  facet_wrap("Strategy")