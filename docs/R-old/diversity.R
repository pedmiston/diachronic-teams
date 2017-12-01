source("docs/R/setup.R")

# ---- diversity-50
data("Teams")
data("TeamPerformance")

DiachronicTeams50Diversities <- Teams %>%
  dplyr::filter(
    Exp == "50LaborMinutes",
    Strategy == "Diachronic"
  ) %>%
  select(TeamID, Diversity, MostExperiencedPlayerGeneration)

DiachronicTeams50 <- TeamPerformance %>%
  dplyr::filter(Strategy == "Diachronic") %>%
  left_join(DiachronicTeams50Diversities)

diversity_mod <- lm(NumInnovations ~ Diversity * MostExperiencedPlayerGeneration, data = DiachronicTeams50)
summary(diversity_mod)

ggplot(DiachronicTeams50) +
  aes(Diversity, NumInnovations, group = MostExperiencedPlayerGeneration) +
  geom_point(aes(color = factor(MostExperiencedPlayerGeneration))) +
  geom_smooth(method = "lm") +
  theme(legend.position = "top")
