source("docs/R/_setup.R")

# ---- exp1

# List to hold descriptives for in-text citation
exp1 <- list()

# Diachronic G2 ----
data("Guesses")
data("Sampled")

Guesses %<>% filter(TeamID != "G47")

DiachronicInheritance <- filter_diachronic_exp(Guesses) %>%
  filter(Generation > 1) %>%
  label_stage_time() %>%
  mutate(StageTime_2 = StageTime * StageTime)

DG1Summary <- Guesses %>%
  filter_diachronic_exp() %>%
  filter(Generation == 1) %>%
  group_by(SessionID) %>%
  summarize(InheritanceSize = max(SessionInventorySize)) %>%
  mutate(NumInnovationsInherited = InheritanceSize - 6)

exp1$mean_inheritance_size <- round(mean(DG1Summary$NumInnovationsInherited), 1)
exp1$sd_inheritance_size <- round(sd(DG1Summary$NumInnovationsInherited), 1)

DiachronicStages <- DiachronicInheritance %>%
  group_by(SessionID) %>%
  summarize(EndLearning = max(SessionTime[Stage == "learning"]))

exp1$mean_learning_time_min <- round(mean(DiachronicStages$EndLearning)/60, 1)
exp1$proportion_learning_time <- round((exp1$mean_learning_time_min/25) * 100, 1)

learning_times_plot <- ggplot(DiachronicStages) +
  aes(EndLearning) +
  geom_histogram(binwidth = 2.5, center = 1.25) +
  scale_x_continuous("Time of learning stage", breaks = seq(0, 25, by = 5))

TimeToRecreateInheritance <- left_join(DG1Summary, DiachronicStages)

time_to_recreate_inheritance_mod <- lm(
  EndLearning ~ NumInnovationsInherited,
  # Remove outliers who took exorbitantly long to recreate their inheritance
  data = dplyr::filter(TimeToRecreateInheritance, EndLearning < 10 * 60)
)
time_to_recreate_inheritance_preds <- get_lm_mod_preds(time_to_recreate_inheritance_mod) %>%
  rename(EndLearning = fit, SE = se.fit)

learning_times_by_inheritance_size_plot <- ggplot(TimeToRecreateInheritance) +
  aes(NumInnovationsInherited, EndLearning) +
  geom_point() +
  geom_smooth(aes(ymin = EndLearning-SE, ymax = EndLearning + SE),
              stat = "identity", data = time_to_recreate_inheritance_preds) +
  scale_x_continuous("Number of items inherited") +
  scale_y_continuous("Time of learning stage (min)", breaks = seq(0, 25, by = 5))

diachronic_player_stages_plot <- ggplot(DiachronicInheritance) +
  aes(SessionTime, SessionInventorySize) +
  geom_line(aes(group = SessionID), color = t_$color_picker("green")) +
  facet_wrap("SessionID") +
  theme(legend.position = "none")

# This plot is broken because some participants get unique items that their ancestors do not.
diachronic_player_stages_plot_labeled <- diachronic_player_stages_plot +
  geom_vline(aes(xintercept = EndLearning),
             data = DiachronicStages,
             color = t_$color_picker("blue"))

illustrative_sessions <- c("G123", "G160", "G164")
illustrative_diachronic_player_stages_plot <- (
  # Replace data in plot with just the data from the illustrative teams
  diachronic_player_stages_plot %+% dplyr::filter(DG2, SessionID %in% illustrative_teams)
) +
  geom_vline(aes(xintercept = EndLearning),
             data = dplyr::filter(DG2Stages, TeamID %in% illustrative_teams),
             color = t_$color_picker("blue")) +
  scale_x_continuous("Session time (min)",
                     breaks = seq(0, 25 * 60, by = 5 * 60),
                     labels = seq(0, 25,      by = 5)) +
  scale_y_continuous("Inventory Size") +
  t_$base_theme +
  theme(legend.position = "none")

DiachronicPlaying <- DiachronicInheritance %>%
  filter(Stage == "playing") %>%
  left_join(DG1Summary)

diachronic_learning_rate_mod <- lmer(
  SessionInventorySize ~ (StageTimeMin + StageTimeMin_2) * InheritanceSize + (StageTimeMin + StageTimeMin_2|TeamID),
  data = DiachronicPlaying
)

mean_inheritance_size <- mean(DG1Summary$InheritanceSize)
sd_inheritance_size <- sd(DG1Summary$InheritanceSize)
sampled_inheritance_sizes <- c(mean_inheritance_size - sd_inheritance_size, mean_inheritance_size, mean_inheritance_size + sd_inheritance_size)

diachronic_learning_rate_preds <- expand.grid(
  StageTimeMin = seq(0, 20, by = 1),
  InheritanceSize = sampled_inheritance_sizes
) %>%
  mutate(StageTimeMin_2 = StageTimeMin * StageTimeMin) %>%
  cbind(., predictSE(diachronic_learning_rate_mod, newdata = ., se = TRUE)) %>%
  rename(SessionInventorySize = fit, SE = se.fit)

diachronic_player_trajectories_plot <- ggplot(DiachronicInheritance) +
  aes(StageTime, SessionInventorySize) +
  geom_line(aes(group = SessionID), color = t_$color_picker("green")) +
  geom_vline(xintercept = 0, color = t_$color_picker("blue")) +
  geom_smooth(aes(ymin = SessionInventorySize - SE, ymax = SessionInventorySize + SE, group = InheritanceSize),
              stat = "identity", data = diachronic_learning_rate_preds,
              color = t_$color_picker("orange")) +
  scale_x_continuous("Playin time (min)",
                     breaks = seq(-25, 25, by = 5),
                     labels = seq(-25, 25, by = 5)) +
  scale_y_continuous("Inventory Size") +
  t_$base_theme +
  ggtitle("All participants")
