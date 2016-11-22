# source("reports/kaggle/R/setup.R")
source("reports/kaggle/R/theme.R")

# ---- team-types
library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(grid)
library(gridExtra)

submission_rates <- list(
  steady = data_frame(
    SubmissionNum = 1:10,
    SubmissionTime = 1:10
  ),
  long = data_frame(
    SubmissionNum = 1:2,
    SubmissionTime = c(1, 10)
  ),
  short = data_frame(
    SubmissionNum = 1:2,
    SubmissionTime = c(1, 2)
  ),
  rapid = data_frame(
    SubmissionNum = 1:10,
    SubmissionTime = seq(1, 2, length.out = 10)
  )
) %>% bind_rows(.id = "TeamType")

team_type_levels <- c("steady", "long", "short", "rapid")
team_type_labels <- c("steady", "long interval", "short", "rapid")
team_type_colors <- unname(colors[c("green", "blue", "pink", "orange")])
team_type_map <- data_frame(
  TeamType = team_type_levels,
  TeamLabel = factor(team_type_levels,
                     levels = team_type_levels,
                     labels = team_type_labels)
)
recode_team_type <- . %>% left_join(team_type_map)

scale_fill_team_type <- scale_fill_manual(values = team_type_colors)
scale_color_team_type <- scale_color_manual(values = team_type_colors)

submission_rates %<>% recode_team_type

# Select first and last points for arrow
arrow_data <- submission_rates %>%
  group_by(TeamType) %>%
  filter(SubmissionNum == min(SubmissionNum) | SubmissionNum == max(SubmissionNum)) %>%
  mutate(Arrow = c("Start", "End")) %>%
  ungroup() %>%
  select(TeamType, Arrow, SubmissionTime) %>%
  spread(Arrow, SubmissionTime) %>%
  recode_team_type()

# Create labels
label_data <- arrow_data %>%
  transmute(TeamType, SubmissionTime = rowMeans(cbind(End, Start))) %>%
  recode_team_type()

# Total time
total_time <- arrow_data %>%
  transmute(TeamType, TotalTime = End - Start) %>%
  recode_team_type()

# Quadrant plot data
team_type_points <- submission_rates %>%
  group_by(TeamType) %>%
  summarize(TotalSubmissions = n(),
            TotalTime = max(SubmissionTime) - min(SubmissionTime)) %>%
  recode_team_type() %>%
  mutate(
    NudgeY = ifelse(TotalTime > 5, -2, 2),
    NudgeX = ifelse(TotalSubmissions > 5, -2, 2)
  )

scale_x_team_label <- scale_x_discrete("")
default_alpha <- 0.6

gg_base <- ggplot(submission_rates, aes(TeamType)) +
  base_theme

gg_timeline <- gg_base +
  geom_segment(
    aes(x = TeamType, xend = TeamType, y = Start, yend = End, color = TeamLabel),
    data = arrow_data, alpha = default_alpha) +
  geom_point(aes(y = SubmissionTime, color = TeamLabel),
             size = 1.8, alpha = default_alpha) +
  geom_text(aes(y = SubmissionTime, label = TeamLabel, color = TeamLabel),
            data = label_data,
            vjust = 2, size = 3) +
  scale_x_discrete("team type", labels = rev(team_type_labels)) +
  scale_y_continuous("competition time", breaks = 1:10) +
  scale_color_team_type +
  coord_flip() +
  base_theme +
  theme(axis.text.y = element_blank())

gg_num_submissions <- ggplot(submission_rates, aes(TeamLabel)) +
  geom_bar(aes(fill = TeamLabel), stat = "count", alpha = default_alpha) +
  scale_x_team_label +
  scale_y_continuous("number of submissions", breaks = 0:10) +
  scale_fill_team_type +
  base_theme

gg_total_time <- ggplot(total_time, aes(TeamLabel, TotalTime)) +
  geom_bar(aes(fill = TeamLabel), stat = "identity", alpha = default_alpha) +
  scale_x_team_label +
  scale_y_continuous("submission interval", breaks = 1:10) +
  scale_fill_manual(values = team_type_colors) +
  base_theme

gg_regions <- ggplot(team_type_points, aes(TotalSubmissions, TotalTime)) +
  geom_point(aes(color = TeamLabel), size = 3, alpha = default_alpha) +
  geom_text(
    aes(x = TotalSubmissions + NudgeX, y = TotalTime + NudgeY,
        label = TeamLabel, color = TeamLabel),
    size = 5) +
  geom_hline(yintercept = median(team_type_points$TotalTime),
             color = "gray") +
  geom_vline(xintercept = median(team_type_points$TotalSubmissions),
             color = "gray") +
  scale_x_continuous("number of submissions", breaks = 1:10) +
  scale_y_continuous("submission interval", breaks = 1:9) +
  scale_color_team_type +
  coord_cartesian(xlim = c(1, 10), ylim = c(0, 10)) +
  base_theme

grid.arrange(gg_timeline, gg_regions,
             gg_num_submissions, gg_total_time,
             nrow = 2)