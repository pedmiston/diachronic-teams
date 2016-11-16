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

# Given the submissions made to a competition, 
get_predicted_places <- function(competition_submissions) {
  place_bins <- leaderboards %>%
    filter(CompetitionId == competition_submissions$CompetitionId[[1]]) %>%
    mutate(
      MinScore = Score,
      MaxScore = lag(Score, default = Inf)
    ) %>%
    select(CompetitionId, MinScore, MaxScore, Place)
  
  get_predicted_place <- function(score) {
    place <- place_bins %>%
      filter(score >= MinScore, score < MaxScore) %>%
      .$Place
    if (length(place) == 0) place <- NA
    place
  }
  
  competition_submissions %>%
    rowwise() %>%
    mutate(PredictedPlace = get_predicted_place(Score))
}

submissions %<>%
  group_by(CompetitionId) %>%
  do({ get_predicted_places(.) })

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

by_submissions <- top_100 %>%
  group_by(TotalSubmissions) %>%
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
scale_x_place <- scale_x_continuous(breaks = c(1, seq(10, 100, by = 10)))
scale_x_team_size <- scale_x_continuous("team size", breaks = 1:4)
scale_y_team_size <- scale_y_continuous("team size", breaks = 1:4)
scale_y_submissions <- scale_y_continuous("submissions", breaks = c(1, seq(5, 100, by = 5)))
scale_y_place <- scale_y_reverse("place", breaks = c(1, seq(10, 100, by = 10)))

# ---- submissions-from-place
ggplot(by_place, aes(Place, TotalSubmissions)) +
  geom_point(aes(color = FirstPlaceTeam), alpha = default_alpha) +
  scale_x_place +
  scale_y_submissions +
  scale_color_manual(values = c(colors[["submissions"]], colors[["first_place"]])) +
  coord_cartesian(xlim = c(1, 100), ylim = c(1, 39)) +
  base_theme +
  ggtitle("Top place teams make more submissions")

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
  base_theme

# ---- place-from-submissions
gg_place_from_submissions <- ggplot(by_submissions, aes(TotalSubmissions, Place)) +
  geom_point(aes(size = PercentTeams), alpha = default_alpha,
             color = colors[["submissions"]]) +
  scale_x_continuous("submissions", breaks = c(1, seq(100, 600, by = 100))) +
  scale_y_place +
  scale_size_continuous("Teams", labels = percent) +
  coord_cartesian(xlim = c(1, 600), ylim = c(1, 100)) +
  base_theme +
  theme(
    legend.position = "bottom"
  ) +
  ggtitle("Making more submissions improves place")

place_mod <- lm(Place ~ TotalSubmissions + TeamSize, data = top_100)

x_preds <- expand.grid(TotalSubmissions = 1:200, TeamSize = 1)
y_preds <- predict(place_mod, x_preds, se = TRUE)
preds <- cbind(x_preds, y_preds) %>%
  rename(Place = fit, SE = se.fit)

gg_place_from_submissions +
  geom_smooth(aes(ymin =  Place - SE, ymax =  Place + SE), data = preds,
              stat = "identity", color = colors[["orange"]])

# ---- teamsize-from-place
ggplot(by_place, aes(Place, TeamSize)) +
  geom_point(aes(color = FirstPlaceTeam), alpha = default_alpha) +
  scale_x_place +
  scale_y_team_size +
  scale_color_manual(values = c(colors[["team_size"]], colors[["first_place"]])) +
  coord_cartesian(xlim = c(1, 100), ylim = c(1:3)) +
  base_theme +
  ggtitle("First place teams are usually bigger")

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
  ) +
  ggtitle("Teammates help a little")

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
  guides(size = "none") +
  ggtitle("By far most submissions are from individuals")

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
