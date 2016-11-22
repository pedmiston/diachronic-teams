source("reports/kaggle/R/setup.R")
source("reports/kaggle/R/theme.R")

# ---- team-types-quartet
submission_rates <- list(
  steady = data_frame(SubmissionNum = 1:10, SubmissionTime = 1:10),
  long   = data_frame(SubmissionNum = 1:2, SubmissionTime = c(1, 10)),
  short  = data_frame(SubmissionNum = 1:2, SubmissionTime = c(1, 2)),
  rapid  = data_frame(SubmissionNum = 1:10,
                      SubmissionTime = seq(1, 2, length.out = 10))
) %>% bind_rows(.id = "TeamType") %>% recode_team_type

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
    NudgeY = ifelse(TotalTime > 5, -1.5, 1.5),
    NudgeX = ifelse(TotalSubmissions > 5, -1.5, 1.5)
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
  base_theme +
  theme(panel.grid.major.x = element_blank())

gg_total_time <- ggplot(total_time, aes(TeamLabel, TotalTime)) +
  geom_bar(aes(fill = TeamLabel), stat = "identity", alpha = default_alpha) +
  scale_x_team_label +
  scale_y_continuous("submission interval", breaks = 1:10) +
  scale_fill_manual(values = team_type_colors) +
  base_theme +
  theme(panel.grid.major.x = element_blank())

gg_regions <- ggplot(team_type_points, aes(TotalSubmissions, TotalTime)) +
  geom_point(aes(color = TeamLabel), size = 3, alpha = default_alpha) +
  geom_text(
    aes(x = TotalSubmissions + NudgeX, y = TotalTime + NudgeY,
        label = TeamLabel, color = TeamLabel),
    size = 3) +
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

# ---- team-types-place
make_rev_rects <- function(frame) {
  width <- 0.9
  baseline <- 100
  frame %>%
    mutate(xmin = TeamNum - width/2, xmax = TeamNum + width/2,
           ymin = Place, ymax = baseline)
}

top_100_team_types <- top_100 %>%
  divide_into_team_types()

team_type_means <- top_100_team_types %>%
  group_by(TeamType) %>%
  summarize(Place = mean(Place)) %>%
  recode_team_type() %>%
  make_rev_rects()

gg_team_types <- ggplot(top_100_team_types, aes(TeamNum, Place)) +
  geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=TeamLabel),
            data = team_type_means, alpha = default_alpha) +
  scale_x_team_num +
  scale_y_reverse("place", breaks = seq(2, 100, by = 2)) +
  scale_color_team_type +
  scale_fill_team_type +
  coord_cartesian(ylim = c(41, 50)) +
  base_theme

team_types_lm_mod <- lm(Place ~ ShortVSteady + ShortVLong + ShortVRapid,
                        data = top_100_team_types)
summary(team_types_lm_mod)

team_types_preds <- recode_team_type() %>%
  cbind(., predict(team_types_lm_mod, newdata = ., se = TRUE)) %>%
  rename(Place = fit, SE = se.fit)

gg_team_types +
  geom_linerange(aes(ymin = Place + SE, ymax = Place - SE),
                 data = team_types_preds)

# ---- team-types-density
new_density_plot <- function(leaderboards) {
  ggplot(leaderboards, aes(TotalSubmissions, TotalTimeSec)) +
    geom_point(aes(color = TeamLabel), alpha = 0.2) +
    geom_hline(yintercept = median(leaderboards$TotalTimeSec),
               color = "gray") +
    geom_vline(xintercept = median(leaderboards$TotalSubmissions),
               color = "gray") +
    scale_x_total_submissions +
    scale_color_team_type +
    make_time_scale("submission interval (days)", seq(0, 400, by = 100)) +
    base_theme
}

new_density_plot(top_100) +
  labs(title = "Team type density for top 100 place finishes")

top_10 <- leaderboards %>%
  filter(Place <= 10) %>%
  label_place_groups()
new_density_plot(top_10) +
  labs(title = "Team type density for top 10 place finishes")

# ---- time-by-submission-top-100
ggplot(top_100_places, aes(TotalSubmissions, TotalTime)) +
  #geom_point(aes(alpha = Place, color = FirstPlaceTeam)) +
  geom_text(aes(label = Place, alpha = Place), size = 2, check_overlap = TRUE) +
  scale_x_continuous("total submissions", breaks = c(1, seq(5, 40, by = 5))) +
  scale_color_manual(values = c("black", colors[["first_place"]])) +
  scale_alpha_continuous(range = c(1.0, 0.2)) +
  coord_cartesian(xlim = c(1, 35), ylim = c(0, 25 * 24 * 3600)) +
  make_time_scale("submission interval (days)", seq(0, 100, by = 5)) +
  base_theme

# ---- prop-time-by-rel-submissions-top-100
