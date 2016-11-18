# ---- setup
library(dplyr)
library(magrittr)

library(ggplot2)
library(gridExtra)
library(scales)

library(lme4)
library(AICcmodavg)

library(evoteams)

db <- connect_kaggle()

# Create map between TeamId -> CompetitionId

team_competitions <- tbl(db, "Teams") %>%
  select(TeamId = Id, CompetitionId)

# Get submissions

submissions <- tbl(db, "Submissions") %>%
  select(TeamId, DateSubmitted, Score = PublicScore) %>%
  left_join(team_competitions) %>%
  as_data_frame() %>%  # break DBI to allow normal data_frame functions, e.g., n()
  group_by(TeamId) %>%
  arrange(DateSubmitted) %>%
  mutate(SubmissionNum = 1:n()) %>%
  ungroup()

# Get leaderboards

# Get final submissions for each team
final_submissions <- submissions %>%
  group_by(TeamId) %>%
  filter(SubmissionNum == max(SubmissionNum)) %>%
  rename(TotalSubmissions = SubmissionNum) %>%
  ungroup()

# Label team size on leaderboards
team_sizes <- tbl(db, "TeamMemberships") %>%
  select(TeamId, UserId) %>%
  count(TeamId) %>%
  rename(TeamSize = n)

final_submissions %<>% left_join(team_sizes, copy = TRUE)

# Create leaderboards
leaderboards <- final_submissions %>%
  group_by(CompetitionId) %>%
  arrange(desc(Score)) %>%  # assumes bigger scores are better
  mutate(Place = 1:n()) %>%
  ungroup() %>%
  as_data_frame()

# Predict submission place based on final leaderboard.

submissions %<>% predict_place(leaderboards)

# Investigate top 100 places only

top_100 <- leaderboards %>%
  filter(Place <= 100) %>%
  mutate(FirstPlaceTeam = Place == 1)

summarize_by_place <- function(frame) {
  frame %>%
    summarize(
      Place = mean(Place),
      NTeams = n()
    ) %>%
    mutate(
      PercentTeams = NTeams/sum(NTeams),
      PercentTeamsLabel = percent(PercentTeams)
    )
}

by_place <- top_100 %>%
  group_by(FirstPlaceTeam, Place) %>%
  summarize(
    TotalSubmissions = mean(TotalSubmissions),
    TeamSize = mean(TeamSize, na.rm = TRUE)
  )

by_team_size <- top_100 %>%
  group_by(TeamSize) %>%
  summarize_by_place()

label_submission_bins <- function(frame) {
  submission_bin_width <- 10
  breaks <- seq(1, max(frame$TotalSubmissions) + 1, by = submission_bin_width)
  labels <- cbind(break_min = breaks, break_max = lead(breaks, n = 1) - 1) %>%
    as_data_frame %>%
    head(-1) %>%  # drop last row
    mutate(break_means = rowMeans(.[, c("break_min", "break_max")])) %>%
    .$break_means
    
  bins <- cut(frame$TotalSubmissions, breaks = breaks, labels = labels, right = FALSE)
  bins <- as.numeric(as.character(bins))  # factor -> character -> numeric
  
  frame %>%
    mutate(TotalSubmissionsBin = bins)
}

by_submissions <- top_100 %>%
  label_submission_bins() %>%
  group_by(TotalSubmissionsBin) %>%
  summarize_by_place()

# theme
base_theme <- theme_minimal() +
  theme(
    legend.position = "none",
    axis.ticks = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(size = 12)
  )

colors <- RColorBrewer::brewer.pal(3, "Set2")
names(colors) <- c("green", "orange", "blue")
colors["first_place"] <- colors[["orange"]]
colors["submissions"] <- colors[["blue"]]
colors["team_size"] <- colors[["green"]]

default_alpha <- 0.6

# scales
scale_x_place <- scale_x_continuous("place", breaks = c(1, seq(10, 100, by = 10)))
scale_x_total_submissions <- scale_x_continuous("total submissions", breaks = c(1, seq(100, 600, by = 100)))
scale_x_submission_number <- scale_x_continuous("submission number", breaks = c(1, seq(100, 600, by = 100)))
scale_x_team_size <- scale_x_continuous("team size", breaks = 1:4)
scale_y_team_size <- scale_y_continuous("team size", breaks = 1:4)
scale_y_submissions <- scale_y_continuous("total submissions", breaks = c(1, seq(5, 100, by = 5)))
scale_y_place <- scale_y_reverse("place", breaks = c(1, seq(10, 100, by = 10)))

# coord cartesian
top_100_submissions_ylim <- c(1, 39)
top_100_places_xlim <- c(1, 100)

# ---- submissions-from-place
ggplot(by_place, aes(Place, TotalSubmissions)) +
  geom_point(aes(color = FirstPlaceTeam), alpha = default_alpha) +
  scale_x_place +
  scale_y_submissions +
  scale_color_manual(values = c(colors[["submissions"]], colors[["first_place"]])) +
  coord_cartesian(xlim = top_100_places_xlim, ylim = top_100_submissions_ylim) +
  base_theme +
  labs(title = "Top place teams make more submissions")

# ---- relative-submissions-from-place
submissions_relative_first_place <- top_100 %>%
  group_by(CompetitionId) %>%
  mutate(
    SubmissionsToFirstPlace = TotalSubmissions - TotalSubmissions[FirstPlaceTeam == TRUE]
  ) %>%
  ungroup()

ggplot(submissions_relative_first_place, aes(Place, SubmissionsToFirstPlace)) +
  geom_point(aes(color = FirstPlaceTeam), alpha = default_alpha,
             stat = "summary", fun.y = "mean") +
  scale_x_place +
  scale_y_continuous("submissions\nrelative to first place team",
                     breaks = seq(-100, 10, by = 5)) +
  scale_color_manual(values = c(colors[["submissions"]], colors[["first_place"]])) +
  coord_cartesian(xlim = top_100_places_xlim, ylim = top_100_submissions_ylim - 40) +
  base_theme +
  labs(title = "Top place teams make more submissions")

# ---- place-from-submissions
gg_place_from_submissions <- ggplot(by_submissions, aes(TotalSubmissionsBin, Place)) +
  geom_point(aes(size = PercentTeams), alpha = default_alpha,
             color = colors[["submissions"]]) +
  scale_x_total_submissions +
  scale_y_place +
  scale_size_continuous("proportion of teams", labels = percent, breaks = rev(c(0.01, 0.05, 0.15, 0.6))) +
  coord_cartesian(xlim = c(1, 600), ylim = c(1, 100)) +
  base_theme +
  theme(legend.position = "bottom") +
  ggtitle("Making more submissions improves place")

place_mod <- lm(Place ~ TotalSubmissions + TeamSize, data = top_100)

x_preds <- expand.grid(TotalSubmissions = 1:200, TeamSize = 1)
y_preds <- predict(place_mod, x_preds, se = TRUE)
preds <- cbind(x_preds, y_preds) %>%
  rename(Place = fit, SE = se.fit) %>%
  mutate(TotalSubmissionsBin = TotalSubmissions)  # for consistency with summary

gg_place_from_submissions +
  geom_smooth(aes(ymin =  Place - SE, ymax =  Place + SE), data = preds,
              stat = "identity", color = colors[["orange"]])

# ---- predicted-place-from-submission

sample_teams <- function(n_teams = 1, min_submissions = 50, 
                         min_final_place = 100, seed = NA) {

  if (!is.na(seed)) set.seed(seed)

  team_ids <- leaderboards %>%
    filter(
      TotalSubmissions >= min_submissions,
      Place <= min_final_place
    ) %>%
    sample_n(n_teams) %>%
    .$TeamId
  
  submissions %>% filter(TeamId %in% team_ids)
}

n_teams <- 200
submissions_sample <- sample_teams(n_teams = n_teams, seed = 821)

team_submissions_plot <- ggplot(submissions_sample, aes(SubmissionNum, PredictedPlace)) +
  geom_smooth(aes(group = TeamId), method = "lm", se = FALSE,
              size = 0.4, alpha = 0.4, color = colors[["submissions"]]) +
  scale_x_submission_number +
  scale_y_reverse("place", breaks = c(1, 500, seq(1000, 5000, by = 1000))) +
  base_theme +
  theme(legend.position = "none") +
  labs(title = paste("Changes in performance for", n_teams, "teams"))

team_submissions_mod <- lmer(PredictedPlace ~ SubmissionNum + 
                               (SubmissionNum|TeamId) + (1|CompetitionId/TeamId),
                             data = submissions_sample)

team_submissions_preds <- data_frame(SubmissionNum = 1:100) %>%
  cbind(., predictSE(team_submissions_mod, newdata = .)) %>%
  rename(PredictedPlace = fit, SE = se.fit)

team_submissions_plot +
  geom_smooth(aes(ymin = PredictedPlace - SE, ymax = PredictedPlace + SE),
              data = team_submissions_preds, color = colors[["orange"]],
              size = 1.5)

# ---- teamsize-from-place
ggplot(by_place, aes(Place, TeamSize)) +
  geom_point(aes(color = FirstPlaceTeam), alpha = default_alpha) +
  scale_x_place +
  scale_y_team_size +
  scale_color_manual(values = c(colors[["team_size"]], colors[["first_place"]])) +
  coord_cartesian(xlim = c(1, 100), ylim = c(1:3)) +
  base_theme

# ---- place-from-teamsize
gg_place_from_teamsize <- ggplot(by_team_size, aes(TeamSize, Place)) +
  geom_point(aes(size = PercentTeams), alpha = default_alpha,
             color = colors[["team_size"]]) +
  scale_x_continuous("team size", breaks = c(1, seq(10, 40, by = 10))) +
  scale_y_place +
  scale_size_continuous("Teams", labels = percent) +
  base_theme +
  theme(
    legend.position = "bottom"
  )

place_mod <- lm(Place ~ TotalSubmissions + TeamSize, data = top_100)

max_team_size <- 5
x_preds <- expand.grid(TotalSubmissions = mean(top_100$TotalSubmissions),
                       TeamSize = 1:max_team_size)
y_preds <- predict(place_mod, x_preds, se = TRUE)
preds <- cbind(x_preds, y_preds) %>%
  rename(Place = fit, SE = se.fit)

gg_place_from_teamsize +
  geom_smooth(aes(ymin = Place - SE, ymax = Place + SE), data = preds,
              stat = "identity", color = colors[["orange"]])

# ---- place-from-teamsize-zoom
gg_place_from_teamsize %+% filter(by_team_size, TeamSize <= max_team_size) +
  geom_smooth(aes(ymin = Place - SE, ymax = Place + SE), data = preds,
              stat = "identity", color = colors[["orange"]]) +
  scale_x_continuous("team size", breaks = 1:max_team_size) +
  geom_text(aes(label = PercentTeamsLabel), nudge_y = 1.2, size = 3) +
  guides(size = "none")

# ---- correlation
place_breaks <- c(100, 50, 25, 1)
first_place <- by_place %>% filter(Place == 1) %>% as.list
ggplot(by_place, aes(TeamSize, TotalSubmissions)) +
  geom_point(aes(color = FirstPlaceTeam), alpha = default_alpha, size = 2) +
  scale_x_continuous("team size", breaks = 1:4) +
  scale_y_continuous("submissions", breaks = c(1, seq(5, 100, by = 5))) +
  scale_color_manual(values = c("gray", colors[["first_place"]])) +
  coord_cartesian(xlim = c(1, 3), ylim = c(1, 39)) +
  base_theme
