library(ggplot2)
library(dplyr)
library(magrittr)

diachronic <- data_frame(
  team_structure = "diachronic",
  projects = "single",
  calendar_hours = 1:8,
  project_name = "a",
  labor_hours = calendar_hours
)

synchronic <- data_frame(
  team_structure = "synchronic",
  projects = "single",
  calendar_hours = 1:2,
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
  calendar_hours = seq(0, 8, by = 0.5),
  project_name = rep(letters[1:4], length.out = length(calendar_hours)),
  labor_hours = calendar_hours * 4
)



time <- rbind(diachronic, synchronic, diachronic_multiple, synchronic_multiple) %>%
  mutate(
    projects_label = factor(projects, levels = c("single", "multiple"),
                            labels = c("Single project", "Multiple projects")),
    project_name = factor(project_name, levels = c("a", "b", "c", "d", "abcd"))
  )

gg_base <- ggplot(time, aes(calendar_hours, labor_hours)) +
  scale_x_continuous("calendar hours", seq(0, 8, by = 2)) +
  scale_y_continuous("labor hours", seq(0, 32, by = 2)) +
  theme_minimal() +
  theme(
    axis.ticks = element_blank()
  )

single_project <- (gg_base %+% filter(time, projects == "single")) +
  geom_line(aes(color = team_structure))

single_project

multiple_projects <- (gg_base %+% filter(time, projects == "multiple")) +
  geom_line(aes(color = project_name)) +
  facet_wrap("team_structure")

multiple_projects