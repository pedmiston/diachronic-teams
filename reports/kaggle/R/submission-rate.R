library(ggplot2)
library(dplyr)
library(tidyr)

submission_rates <- list(
  steady = data_frame(
    SubmissionNum = 1:10,
    SubmissionTime = 0:9
  ),
  long = data_frame(
    SubmissionNum = 1:2,
    SubmissionTime = c(0, 9)
  ),
  gave_up = data_frame(
    SubmissionNum = 1:2,
    SubmissionTime = c(0, 1)
  )
) %>% bind_rows(.id = "TeamType")

# Select first and last points for arrow
arrow_data <- submission_rates %>%
  group_by(TeamType) %>%
  filter(SubmissionNum == min(SubmissionNum) | SubmissionNum == max(SubmissionNum)) %>%
  mutate(Arrow = c("Start", "End")) %>%
  ungroup() %>%
  select(TeamType, Arrow, SubmissionTime) %>%
  spread(Arrow, SubmissionTime)

# Create labels
label_data <- arrow_data %>%
  transmute(TeamType, SubmissionTime = rowMeans(cbind(End, Start)))

# Total time
total_time <- arrow_data %>%
  transmute(TeamType, TotalTime = End - Start)

ggplot(submission_rates, aes(SubmissionTime, TeamType)) +
  geom_point(size = 3, alpha = 0.6) +
  geom_segment(aes(x = Start, xend = End, y = TeamType, yend = TeamType),
               data = arrow_data, arrow = arrow()) +
  geom_text(aes(label = TeamType), data = label_data) +
  scale_x_continuous(breaks = 1:10) +
  theme_void() +
  theme(axis.text.x = element_text())

ggplot(submission_rates, aes(TeamType)) +
  geom_bar(stat = "count") +
  scale_y_continuous("Number of submissions", breaks = 1:10)

ggplot(total_time, aes(TeamType, TotalTime)) +
  geom_bar(stat = "identity") +
  scale_y_continuous("Total time", breaks = 1:10)