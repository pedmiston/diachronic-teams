# Types of time ----
# Makes "types of time" plot.
# Types of time plot shows the relationship
# between labor time, calendar time, and learning time.

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
    LaborHours = ifelse(Strategy == "Synchronic", LaborHours,
                        ifelse(Strategy == "Diachronic", LaborHours + 0.8, LaborHours - 0.8))
  )

axis_breaks <- c(0, 50, 100)
axis_labels <- c(0, expression(1/2), 1)

gg_time <- ggplot(time, aes(CalendarHours, LaborHours)) +
  geom_line(aes(color = StrategyLabel), size = 1.2) +
  scale_x_continuous("Calendar time", breaks = axis_breaks, labels = axis_labels) +
  scale_y_continuous("Labor time", breaks = axis_breaks, labels = axis_labels) +
  t_$scale_color_strategy +
  scale_linetype_manual(values = c(1, 1, 2)) +
  guides(color = guide_legend("", reverse = TRUE), linetype = "none") +
  t_$base_theme +
  theme(legend.position = c(0.75, 0.2))

time %<>%
  mutate(StrategyIsolated = factor(Strategy, levels = c("Synchronic", "Diachronic", "Isolated")))

gg_person <- ggplot(time) +
  aes(StrategyIsolated, PersonHours) +
  geom_bar(aes(fill = StrategyLabel), stat = "summary", fun.y = "max",
           alpha = 0.8) +
  scale_x_discrete("") +
  scale_y_continuous("Learning time", breaks = axis_breaks, labels = axis_labels) +
  t_$scale_fill_strategy +
  t_$base_theme +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank())

# Combinations ----
count_unique_guesses <- function(n_items) {
  n_guesses_of_size <- function(guess_size) {
    nrow(expand.grid(rep(list(1:n_items), guess_size)))
  }
  sum(sapply(1:4, n_guesses_of_size))
}