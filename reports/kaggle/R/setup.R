# ---- setup
library(dplyr)
library(magrittr)
library(lubridate)

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
  mutate(DateSubmitted = ymd_hms(DateSubmitted)) %>%
  group_by(TeamId) %>%
  arrange(DateSubmitted) %>%
  mutate(SubmissionNum = 1:n()) %>%
  ungroup()

# Get leaderboards (one observation per team)
leaderboards <- submissions %>%
  # Select the final submission for each team.
  group_by(TeamId) %>%
  filter(SubmissionNum == max(SubmissionNum)) %>%
  rename(TotalSubmissions = SubmissionNum) %>%
  ungroup() %>%
  # Assign places for each competition.
  group_by(CompetitionId) %>%
  arrange(desc(Score)) %>%  # assumes bigger scores are better
  mutate(Place = 1:n()) %>%
  ungroup() %>%
  as_data_frame()

# Determine team sizes
team_sizes <- tbl(db, "TeamMemberships") %>%
  select(TeamId, UserId) %>%
  count(TeamId) %>%
  rename(TeamSize = n)
leaderboards %<>% left_join(team_sizes, copy = TRUE)

# Measure submission intervals
total_times <- submissions %>% 
  time_interval()
leaderboards %<>% left_join(total_times)

# Predict submission place based on final leaderboard.
submissions %<>% predict_place(leaderboards)

# Investigate top 100 places only
top_100 <- leaderboards %>%
  filter(Place <= 100) %>%
  mutate(FirstPlaceTeam = Place == 1)

summarize_by_place <- . %>%
  summarize(
    Place = mean(Place),
    TotalTime = mean(TotalTime),
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
    TeamSize = mean(TeamSize, na.rm = TRUE),
    TotalTime = mean(TotalTime)
  )

by_team_size <- top_100 %>%
  group_by(TeamSize) %>%
  summarize_by_place()

by_submissions <- top_100 %>%
  label_submission_bins() %>%
  group_by(TotalSubmissionsBin) %>%
  summarize_by_place()
