source("docs/R/setup.R")

# ---- inheritances-50
data("PlayerPerformance")

PlayerPerformance50 <- PlayerPerformance %>%
  dplyr::filter(
    TeamStatus == "V",
    Strategy != "Synchronic",
    SessionDuration == 25,
    Exp == "50LaborMinutes"
  ) %>%
  recode_strategy()

Inheritance50 <- PlayerPerformance50 %>%
  group_by(TeamID) %>%
  summarize(
    InheritedInventoryID = FinalInventoryID[Generation == 1],
    NumInheritedInnovations = NumInnovations[Generation == 1],
    FinalInventoryID = FinalInventoryID[Generation == 2],
    NumFinalInnovations = NumInnovations[Generation == 2]
  ) %>%
  ungroup() %>%
  mutate(NumAdditionalInnovations = NumFinalInnovations - NumInheritedInnovations) %>%
  left_join(dplyr::filter(Teams, Exp == "50LaborMinutes"))
  
AdditionalInnovations <- Inheritance50 %>%
  group_by(Strategy, InheritedInventoryID, NumInheritedInnovations) %>%
  summarize(
    NumAdditionalInnovations = mean(NumAdditionalInnovations),
    NObs = n()
  )

addition_innovations_mod <- lm(
  NumAdditionalInnovations ~ NumInheritedInnovations * Strategy,
  data = AdditionalInnovations
)

ggplot(AdditionalInnovations) +
  aes(NumInheritedInnovations, NumAdditionalInnovations, color = Strategy) +
  geom_point(aes(size = NObs)) +
  geom_smooth(method = "lm", se = FALSE) +
  coord_cartesian(xlim = c(0, 22.5), ylim = c(0, 22.5))
