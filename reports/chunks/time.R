# ---- time
library(ggplot2)
library(dplyr)
library(magrittr)

library(evoteams)

diachronic <- data_frame(
  team_structure = "diachronic",
  projects = "single",
  calendar_hours = 0:8,
  project_name = "a",
  labor_hours = calendar_hours
)

synchronic <- data_frame(
  team_structure = "synchronic",
  projects = "single",
  calendar_hours = 0:2,
  project_name = "a",
  labor_hours = calendar_hours * 4
)

diachronic_multiple <- data_frame(
  team_structure = "diachronic",
  projects = "multiple",
  calendar_hours = 0:8,
  project_name = "abcd",
  labor_hours = calendar_hours * 4
)

synchronic_multiple <- data_frame(
  team_structure = "synchronic",
  projects = "multiple",
  calendar_hours = seq(0, 8, length.out = 20),
  project_name = rep(letters[1:4], each = 5),
  labor_hours = calendar_hours * 4
)

time <- rbind(diachronic, synchronic, diachronic_multiple, synchronic_multiple) %>%
  recode_team_structures %>%
  mutate(
    projects_label = factor(projects, levels = c("single", "multiple"),
                            labels = c("Single project", "Multiple projects")),
    project_name = factor(project_name, levels = c("a", "b", "c", "d", "abcd"))
  )

colors <- RColorBrewer::brewer.pal(4, "Set2")
names(colors) <- c("green", "orange", "blue", "pink")
team_colors <- c(colors[["green"]], colors[["blue"]])

line_size <- 1.5

time_plot <- ggplot(time, aes(calendar_hours, labor_hours)) +
  scale_x_continuous("Calendar hours", seq(0, 8, by = 2)) +
  scale_y_continuous("Labor hours", seq(0, 32, by = 2)) +
  theme_minimal() +
  theme(
    axis.ticks = element_blank()
  )

# ---- hours-per-team
(time_plot %+% filter(time, projects == "single")) +
  geom_line(aes(color = team_factor), size = line_size) +
  scale_color_manual("", values = team_colors) +
  coord_cartesian(ylim = c(0, 8), xlim = c(0, 8)) +
  theme(legend.position = "top")

# ---- hours-for-multiple-projects
(time_plot %+% filter(time, projects == "multiple")) +
  geom_line(aes(color = project_name), size = line_size) +
  scale_color_manual("Problem", values = c(unname(colors), "gray")) +
  facet_wrap("team_structure")