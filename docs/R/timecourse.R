source("docs/R/setup.R")

# ---- timecourse-50
data("Sampled")

Sampled <- Sampled %>%
  filter(
    TeamStatus == "V",
    Exp == "50LaborMinutes"
  ) %>%
  recode_strategy()

ggplot(Sampled) +
  aes(TeamTime, InventorySize,
      group = interaction(Strategy, SessionDuration, Generation),
      color = Strategy, linetype = factor(SessionDuration)) +
  geom_line(stat = "summary", fun.y = "mean")

# ---- timecourse-100
data("Sampled")

Sampled <- Sampled %>%
  dplyr::filter(
    TeamStatus == "V",
    Exp == "100LaborMinutes"
  ) %>%
  recode_strategy()

ggplot(Sampled) +
  aes(TeamTime, InventorySize,
      group = interaction(Strategy, SessionDuration, Generation),
      color = Strategy, linetype = factor(SessionDuration)) +
  geom_line(stat = "summary", fun.y = "mean")

# ---- timecourse-100-calendar
data("Sampled")

Sampled <- Sampled %>%
  dplyr::filter(
    TeamStatus == "V",
    Exp == "100LaborMinutes"
  ) %>%
  recode_strategy()

ggplot(Sampled) +
  aes(CalendarTime, InventorySize,
      group = interaction(Strategy, SessionDuration, Generation),
      color = Strategy, linetype = factor(SessionDuration)) +
  geom_line(stat = "summary", fun.y = "mean")

# ---- diachronic-performance
data("Sampled")

Sampled <- Sampled %>%
  filter(
    TeamStatus == "V",
    Exp == "100LaborMinutes",
    Strategy == "Diachronic"
  )

ggplot(Sampled) +
  aes(SessionTime, InventorySize, group = Generation) +
  geom_line() +
  facet_wrap("TeamID")

ggplot(Sampled) +
  aes(SessionTime, InventorySize, group = Generation, color = Generation) +
  geom_line(stat = "summary", fun.y = "mean")

# ---- single-player-performance
data("Sampled")

Sampled <- Sampled %>%
  filter(
    TeamStatus == "V",
    Exp == "100LaborMinutes",
    Strategy != "Synchronic"
  )

ggplot(Sampled) +
  aes(SessionTime, InventorySize, group = Generation, color = factor(Generation)) +
  geom_line(stat = "summary", fun.y = "mean") +
  facet_wrap("Strategy")
