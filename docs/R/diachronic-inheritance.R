source("docs/R/setup.R")

# ---- diachronic-inheritance
data("Guesses")
data("Players")

# Diachronic players ----
DiachronicPlayers <- Players %>%
  filter(Strategy == "Diachronic", Exp == "100LaborMinutes", TeamStatus == "V")

DiachronicGuesses <- Guesses %>%
  filter(Strategy == "Diachronic", Exp == "100LaborMinutes", TeamStatus == "V") %>%
  arrange(SessionTime) %>%
  group_by(SessionID) %>%
  mutate(
    Stage = ifelse(nchar(PrevTeamInventoryID) > nchar(PrevSessionInventoryID),
                   "learning", "playing")
  ) %>%
  group_by(SessionID, Stage) %>%
  do({
    stage <- .data$Stage[[1]]
    if(stage == "learning") {
      result <- .data %>%
        arrange(desc(SessionTime)) %>%
        mutate(StageIX = -cumsum(UniqueSessionResult))
    } else if(stage == "playing") {
      result <- .data %>%
        arrange(SessionTime) %>%
        mutate(StageIX = cumsum(UniqueSessionResult))
    }
    result
  })

DiachronicStages <- DiachronicGuesses %>%
  group_by(SessionID, StageIX) %>%
  summarize(
    NumGuesses = n()
  ) %>%
  left_join(DiachronicPlayers) %>%
  highlight_inheritance_100() %>%
  filter(!is.na(AllInheritance))

DiachronicStagesModData <- DiachronicStages %>%
  filter(StageIX >= 1) %>%
  mutate(StageIX_2 = StageIX^2)

diachronic_stages_mod <- lmer(
  NumGuesses ~ AllInheritance * (StageIX + StageIX_2) +
    (StageIX + StageIX_2|PlayerID),
  data = DiachronicStagesModData
)

diachronic_stages_preds <- expand.grid(
  StageIX = 1:10,
  AllInheritance = c("diachronic_inheritance", "no_inheritance"),
  stringsAsFactors = FALSE
) %>%
  mutate(StageIX_2 = StageIX^2) %>%
  cbind(., predictSE(diachronic_stages_mod, newdata = ., se = TRUE)) %>%
  rename(NumGuesses = fit, SE = se.fit)

diachronic_stages_plot <- ggplot(DiachronicStages) +
  aes(StageIX, NumGuesses, color = AllInheritance) +
  geom_line(aes(group = AllInheritance), stat = "summary", fun.y = "mean") +
  geom_smooth(aes(ymin = NumGuesses - SE, ymax = NumGuesses + SE),
              stat = "identity", data = diachronic_stages_preds) +
  scale_x_continuous("Size of inventory relative to ancestor") +
  scale_y_continuous("Number of guesses") +
  scale_color_manual("", labels = c("Generation > 1", "Generation 1"),
                     values = totems_theme$color_picker(c("green", "blue"))) +
  guides(color = guide_legend(reverse = TRUE)) +
  totems_theme$base_theme +
  theme(legend.position = c(0.85, 0.8)) +
  ggtitle("Diachronic inheritance")


# Isolated players ----
IsolatedPlayers <- Players %>%
  filter(Strategy == "Isolated", Exp == "100LaborMinutes", TeamStatus == "V")

IsolatedGuesses <- Guesses %>%
  filter(Strategy == "Isolated", Exp == "100LaborMinutes", TeamStatus == "V") %>%
  arrange(SessionTime) %>%
  group_by(SessionID) %>%
  mutate(
    Stage = ifelse(nchar(PrevTeamInventoryID) > nchar(PrevSessionInventoryID),
                   "learning", "playing")
  ) %>%
  group_by(SessionID, Stage) %>%
  do({
    stage <- .data$Stage[[1]]
    if(stage == "learning") {
      result <- .data %>%
        arrange(desc(SessionTime)) %>%
        mutate(StageIX = -cumsum(UniqueSessionResult))
    } else if(stage == "playing") {
      result <- .data %>%
        arrange(SessionTime) %>%
        mutate(StageIX = cumsum(UniqueSessionResult))
    }
    result
  })

IsolatedStages <- IsolatedGuesses %>%
  group_by(SessionID, StageIX) %>%
  summarize(
    NumGuesses = n()
  ) %>%
  left_join(IsolatedPlayers) %>%
  mutate(Generation1 = Generation == 1) %>%
  filter(!is.na(Generation1))

IsolatedStagesModData <- IsolatedStages %>%
  filter(StageIX >= 1) %>%
  mutate(StageIX_2 = StageIX^2)

isolated_stages_mod <- lmer(
  NumGuesses ~ Generation1 * (StageIX + StageIX_2) +
    (StageIX + StageIX_2|PlayerID),
  data = IsolatedStagesModData
)

isolated_stages_preds <- expand.grid(
  StageIX = 1:10,
  Generation1 = c(TRUE, FALSE),
  stringsAsFactors = FALSE
) %>%
  mutate(StageIX_2 = StageIX^2) %>%
  cbind(., predictSE(isolated_stages_mod, newdata = ., se = TRUE)) %>%
  rename(NumGuesses = fit, SE = se.fit)

isolated_stages_plot <- ggplot(IsolatedStages) +
  aes(StageIX, NumGuesses, color = factor(Generation1)) +
  geom_line(aes(group = Generation1), stat = "summary", fun.y = "mean") +
  geom_smooth(aes(ymin = NumGuesses - SE, ymax = NumGuesses + SE),
              stat = "identity", data = isolated_stages_preds) +
  scale_x_continuous("Size of inventory relative to ancestor") +
  scale_y_continuous("Number of guesses") +
  scale_color_manual("", labels = c("Generation > 1", "Generation 1"),
                     values = totems_theme$color_picker(c("green", "blue"))) +
  guides(color = guide_legend(reverse = TRUE)) +
  totems_theme$base_theme +
  theme(legend.position = c(0.85, 0.8)) +
  ggtitle("Repeat sessions")
