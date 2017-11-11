library(tidyverse)
library(magrittr)
library(totems)

# ---- types-of-time
totems_theme <- load_totems_theme()

diachronic <- data_frame(
  Strategy = "Diachronic",
  CalendarHours = 1:100,
  LaborHours = CalendarHours,
  Person = rep(c(1, 2), each = 50)
)

isolated <- diachronic %>%
  mutate(
    Strategy = "Isolated",
    Person = 1
  )

synchronic <- data_frame(
  Strategy = "Synchronic",
  CalendarHours = 1:50,
  LaborHours = CalendarHours * 2,
  Person = 1
)

time <- rbind(diachronic, synchronic, isolated) %>%
  group_by(Strategy, Person) %>%
  mutate(PersonHours = 1:n()) %>%
  ungroup() %>%
  recode_strategy() %>%
  mutate(
    # Separate Diachronic and Isolated lines
    LaborHours = ifelse(Strategy == "Synchronic", LaborHours,
                 ifelse(Strategy == "Diachronic", LaborHours + 0.8, LaborHours - 0.8))
  )

axis_breaks <- c(0, 50, 100)
axis_labels <- c(0, expression(1/2), 1)

gg_time <- ggplot(time, aes(CalendarHours, LaborHours)) +
  geom_line(aes(color = StrategyLabel), size = 1.2) +
  scale_x_continuous("Calendar time", breaks = axis_breaks, labels = axis_labels) +
  scale_y_continuous("Labor time", breaks = axis_breaks, labels = axis_labels) +
  totems_theme["scale_color_strategy"] +
  scale_linetype_manual(values = c(1, 1, 2)) +
  guides(color = guide_legend("", reverse = TRUE), linetype = "none") +
  totems_theme["base_theme"] +
  theme(legend.position = c(0.75, 0.2))

time %<>%
  mutate(StrategyIsolated = factor(Strategy, levels = c("Synchronic", "Diachronic", "Isolated")))

gg_person <- ggplot(time) +
  aes(StrategyIsolated, PersonHours) +
  geom_bar(aes(fill = StrategyLabel), stat = "summary", fun.y = "max",
           alpha = 0.8) +
  scale_x_discrete("") +
  scale_y_continuous("Learning time", breaks = axis_breaks, labels = axis_labels) +
  totems_theme["scale_fill_strategy"] +
  totems_theme["base_theme"] +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank())

types_of_time_plot <- gridExtra::grid.arrange(
  gg_time,
  gg_person,
  nrow = 1
)
