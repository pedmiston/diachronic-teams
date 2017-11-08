source("docs/R/setup.R")

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
