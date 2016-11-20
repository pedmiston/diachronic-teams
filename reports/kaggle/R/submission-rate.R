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

gg_timeline <- ggplot(submission_rates, aes(SubmissionTime, TeamType, color = TeamType)) +
  geom_segment(aes(x = Start, xend = End, y = TeamType, yend = TeamType),
               data = arrow_data, alpha = default_alpha) +
  geom_point(size = 3, alpha = default_alpha) +
  geom_text(aes(label = TeamLabel), data = label_data, vjust = 2,
            size = 3) +
  scale_x_continuous("competition time", breaks = 1:10) +
  scale_y_discrete("") +
  scale_color_brewer(palette = "Set2") +
  base_theme +
  ggtitle("Timeline")

gg_num_submissions <- ggplot(submission_rates, aes(TeamLabel)) +
  geom_bar(aes(fill = TeamType), stat = "count", alpha = default_alpha) +
  scale_x_team_label +
  scale_y_continuous("", breaks = 0:10) +
  scale_fill_brewer(palette = "Set2") +
  base_theme +
  ggtitle("Number of submissions")

gg_total_time <- ggplot(total_time, aes(TeamLabel, TotalTime)) +
  geom_bar(aes(fill = TeamType), stat = "identity", alpha = default_alpha) +
  scale_x_team_label +
  scale_y_continuous("", breaks = 1:10) +
  scale_fill_brewer(palette = "Set2") +
  base_theme +
  ggtitle("Total time")

align_ggplots <- function(x, y, z) {
  to_gtable <- . %>% ggplot_build %>% ggplot_gtable

  x <- to_gtable(x)
  y <- to_gtable(y)
  z <- to_gtable(z)
  
  max_height <- unit.pmin(x$heights[2:3], y$heights[2:3], z$heights[2:3])
  
  x$heights[2:3] <- max_height
  y$heights[2:3] <- max_height
  z$heights[2:3] <- max_height
  
  arrangeGrob(x, y , z, nrow = 1)
}

grid.newpage()
grid.draw(align_ggplots(gg_timeline, gg_num_submissions, gg_total_time))

# ----
submission_rate <- top_100 %>%
  label_submission_bins() %>%
  group_by(TotalSubmissionsBin) %>%
  summarize(
    Total
  )



ggplot(by_submissions, aes(TotalSubmissionsBin, TotalTime)) +
  geom_point(aes(alpha = Place, size = PercentTeams)) +
  scale_alpha_continuous(range = c(1, 0.1)) +
  scale_y_time(breaks = days(seq(0, 150, by = 50))) +
  guides(size = "none")
