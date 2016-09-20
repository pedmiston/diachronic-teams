# ---- hypotheses
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(gridExtra)

library(evoteams)

team_structures <- c("synchronic", "diachronic")
feedback_types <- c("yes", "no", "enforce")

# theme
default_alpha <- 0.8
default_bar_width <- 0.98

base_theme <- theme_minimal() +
  theme(
    axis.ticks = element_blank(),
    panel.grid.major.x = element_blank()
  )

colors <- RColorBrewer::brewer.pal(3, "Set2")
names(colors) <- c("green", "orange", "blue")
team_colors <- c(colors[["green"]], colors[["blue"]])

scale_y_classification_accuracy <- scale_y_continuous("Classification accuracy", labels = percent)
scale_fill_team_factor <- scale_fill_manual(values = team_colors)

base_plot <- ggplot() +
  base_theme

# ---- team-structure-results
exp1 <- data_frame(
  team_structure = team_structures,
  classification_accuracy = c(0.62, 0.81)
) %>% recode_team_structures

(base_plot %+% exp1) +
  geom_bar(aes(team_factor, classification_accuracy, fill = team_factor),
           stat = "identity", width = default_bar_width, alpha = default_alpha) +
  xlab("") +
  scale_y_classification_accuracy +
  scale_fill_team_factor +
  guides(fill = "none") +
  ggtitle("Adaptive problems")

# ---- submissions-per-team
diachronic <- data_frame(
  team_structure = "diachronic",
  calendar_hours = 0:8,
  labor_hours = calendar_hours
)

synchronic <- data_frame(
  team_structure = "synchronic",
  calendar_hours = 0:2,
  labor_hours = calendar_hours * 4
)

hours <- rbind(diachronic, synchronic) %>%
  recode_team_structures

hours_plot <- (base_plot %+% hours) +
  geom_line(aes(calendar_hours, labor_hours, color = team_factor),
            size = 1.5) +
  scale_color_manual("", values = team_colors) +
  coord_cartesian(ylim = c(0, 8), xlim = c(0, 8)) +
  scale_x_continuous("Calendar hours", seq(0, 8, by = 2)) +
  scale_y_continuous("Labor hours", seq(0, 32, by = 2)) +
  theme(legend.position = "top")

submissions <- data_frame(
  team_structure = team_structures,
  submissions = c(8.3, 15.2)
) %>% recode_team_structures

submissions_plot <- (base_plot %+% submissions) +
  geom_bar(aes(team_factor, submissions, fill = team_factor),
           stat = "identity", width = default_bar_width, alpha = default_alpha) +
  xlab("") +
  ylab("Submissions") +
  scale_fill_team_factor +
  guides(fill = "none")

grid.arrange(hours_plot, submissions_plot, nrow = 1)

# ---- delegation
delegation <- data_frame(
  team_structure = team_structures,
  pct_files_changed = c(0.32, 0.51)
) %>% recode_team_structures

(base_plot %+% delegation) +
  geom_bar(aes(team_factor, pct_files_changed, fill = team_factor), stat = "identity",
           width = default_bar_width, alpha = default_alpha) +
  xlab("") +
  scale_y_continuous("Files changed per team member", labels = percent) +
  scale_fill_team_factor +
  guides(fill = "none") +
  ggtitle("Delegation")

# ---- feedback-results
exp2 <- expand.grid(
  feedback_type = feedback_types,
  team_structure = team_structures
)
# > exp2
#   feedback_type team_structure
# 1           yes     synchronic
# 2            no     synchronic
# 3       enforce     synchronic
# 4           yes     diachronic
# 5            no     diachronic
# 6       enforce     diachronic
exp2$classification_accuracy <- c(62, 58, 78, 81, 69, 83)/100
exp2 %<>%
  recode_team_structures %>%
  recode_feedback_types

exp2_treatment_no <- exp2 %>%
  filter(feedback_type %in% c("yes", "no"))

exp2_treatment_yes <- exp2 %>%
  filter(feedback_type %in% c("yes", "enforce"))

treatment_plot <- base_plot +
  geom_bar(aes(feedback_factor, classification_accuracy, fill = team_factor),
           stat = "identity", width = default_bar_width, alpha = default_alpha) +
  facet_wrap("team_factor", nrow = 1) +
  xlab("Feedback") +
  scale_y_classification_accuracy +
  scale_fill_team_factor +
  guides(fill = "none")

(treatment_plot %+% exp2_treatment_no) +
  ggtitle("Removing feedback")

(treatment_plot %+% exp2_treatment_yes) +
  ggtitle("Enforcing feedback")

# ---- insight-results
exp3 <- data_frame(
  team_structure = team_structures,
  labor_hours_to_solution = c(5.8, 7.1)
) %>%
  recode_team_structures

(base_plot %+% exp3) +
  geom_bar(aes(team_factor, labor_hours_to_solution, fill = team_factor),
           stat = "identity", width = default_bar_width, alpha = default_alpha) +
  xlab("") +
  scale_y_continuous("Labor hours to solution", breaks = 1:8) +
  scale_fill_team_factor +
  guides(fill = "none") +
  ggtitle("Insight problems")

# ---- multiple-projects-results
exp4 <- expand.grid(
  team_structure = team_structures,
  problem_id = letters[1:4]
)
# > exp4
#   team_structure problem_id
# 1     synchronic          a
# 2     diachronic          a
# 3     synchronic          b
# 4     diachronic          b
# 5     synchronic          c
# 6     diachronic          c
# 7     synchronic          d
# 8     diachronic          d
exp4$classification_accuracy <- c(72, 78, 62, 68, 81, 84, 74, 76)/100
exp4 %<>%
  recode_team_structures

(base_plot %+% exp4) +
  geom_line(aes(team_factor, classification_accuracy, group = problem_id, color = problem_id),
            size = 1, alpha = default_alpha) +
  geom_point(aes(team_factor, classification_accuracy, shape = problem_id, color = problem_id),
             size = 3) +
  xlab("") +
  scale_y_classification_accuracy +
  scale_color_brewer("Problem", palette = "Set2") +
  scale_shape_manual("Problem", values = c(19, 15, 18, 17)) +
  theme(legend.position = "top") +
  ggtitle("Multiple adaptive problems")