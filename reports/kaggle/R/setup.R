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

summarize_by_place <- . %>%
  summarize(
    Place = mean(Place),
    NTeams = n()
  ) %>%
  mutate(
    PercentTeams = NTeams/sum(NTeams),
    PercentTeamsLabel = percent(PercentTeams)
  )

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