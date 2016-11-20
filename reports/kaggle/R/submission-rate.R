source("reports/kaggle/R/setup.R")
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
  gave_up = data_frame(
    SubmissionNum = 1:2,
    SubmissionTime = c(1, 2)
  )
) %>% bind_rows(.id = "TeamType")

team_type_levels <- c("steady", "long", "gave_up")
team_type_labels <- c("steady", "long interval", "gave up")
team_type_map <- data_frame(
  TeamType = team_type_levels,
  TeamLabel = factor(team_type_levels,
                     levels = team_type_levels,
                     labels = team_type_labels)
)
recode_team_type <- . %>% left_join(team_type_map)

submission_rates %<>% recode_team_type

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
  transmute(TeamType, SubmissionTime = rowMeans(cbind(End, Start))) %>%
  recode_team_type()

# Total time
total_time <- arrow_data %>%
  transmute(TeamType, TotalTime = End - Start) %>%
  recode_team_type()

scale_x_team_label <- scale_x_discrete("")
default_alpha <- 0.6

gg_base <- ggplot(submission_rates, aes(TeamType)) +
  base_theme

gg_timeline <- gg_base +
  geom_segment(aes(x = TeamType, xend = TeamType,
                   y = Start, yend = End,
                   color = TeamType),
               data = arrow_data, alpha = default_alpha) +
  geom_point(aes(y = SubmissionTime, color = TeamType), size = 3, alpha = default_alpha) +
  geom_text(aes(y = SubmissionTime, label = TeamLabel, color = TeamType), data = label_data,
            vjust = 2, size = 3) +
  scale_x_discrete("team type", labels = rev(team_type_labels)) +
  scale_y_continuous("competition time", breaks = 1:10) +
  scale_color_brewer(palette = "Set2") +
  coord_flip() +
  base_theme +
  theme(axis.text.y = element_blank())

gg_num_submissions <- ggplot(submission_rates, aes(TeamLabel)) +
  geom_bar(aes(fill = TeamType), stat = "count", alpha = default_alpha) +
  scale_x_team_label +
  scale_y_continuous("number of submissions", breaks = 0:10) +
  scale_fill_brewer(palette = "Set2") +
  base_theme

gg_total_time <- ggplot(total_time, aes(TeamLabel, TotalTime)) +
  geom_bar(aes(fill = TeamType), stat = "identity", alpha = default_alpha) +
  scale_x_team_label +
  scale_y_continuous("total time", breaks = 1:10) +
  scale_fill_brewer(palette = "Set2") +
  base_theme

team_type_points <- submission_rates %>%
  group_by(TeamType) %>%
  summarize(TotalSubmissions = n(),
            TotalTime = max(SubmissionTime) - min(SubmissionTime)) %>%
  recode_team_type()

gg_regions <- ggplot(team_type_points, aes(TotalSubmissions, TotalTime)) +
  geom_point(aes(color = TeamType), size = 3, shape = 1, stroke = 1.5,
             alpha = default_alpha) +
  geom_text(aes(label = TeamLabel, color = TeamType), size = 3, nudge_y = -1) +
  scale_x_continuous("number of submissions", breaks = 1:10) +
  scale_y_continuous("total time", breaks = 1:9) +
  scale_color_brewer(palette = "Set2") +
  coord_cartesian(xlim = c(1, 10), ylim = c(0, 10)) +
  base_theme

grid.arrange(gg_timeline, gg_regions,
             gg_num_submissions, gg_total_time,
             nrow = 2)

# ---- time-by-submission-density
ggplot(top_100, aes(TotalSubmissions, TotalTimeSec)) +
  geom_point(shape = 1, color = "gray") +
  scale_x_total_submissions +
  make_time_scale("submission interval (days)", seq(0, 400, by = 100)) +
  base_theme

# ---- time-by-submission-bins
ggplot(by_submissions, aes(TotalSubmissionsBin, TotalTime)) +
  geom_point(aes(alpha = Place, size = PercentTeams)) +
  scale_x_total_submissions +
  scale_y_total_time +
  scale_alpha_continuous("final place", range = c(1, 0.2)) +
  scale_size_continuous("proportion teams", breaks = c(0.01, 0.10, 0.50),
                        labels = percent) +
  base_theme +
  theme(legend.position = "bottom")

# ---- time-by-submission-top-100
ggplot(by_place, aes(TotalSubmissions, TotalTime)) +
  geom_point(aes(alpha = Place, color = FirstPlaceTeam)) +
  scale_x_continuous("total submissions", breaks = c(1, seq(5, 40, by = 5))) +
  scale_color_manual(values = c("black", colors[["first_place"]])) +
  scale_alpha_continuous(range = c(1.0, 0.2)) +
  coord_cartesian(xlim = c(1, 35), ylim = c(0, 25 * 24 * 3600)) +
  make_time_scale("submission interval (days)", seq(0, 100, by = 5)) +
  base_theme
