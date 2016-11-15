library(magrittr)
library(dplyr)
library(devtools)

db <- src_sqlite("data-raw/kaggle/database.sqlite")
# src_tbls(db)

teams <- tbl(db, "Teams") %>%
  select(TeamId = Id, CompetitionId)

team_sizes <- tbl(db, "TeamMemberships") %>%
  select(TeamId, UserId) %>%
  count(TeamId) %>%
  rename(TeamSize = n)

n_submissions <- tbl(db, "Submissions") %>%
  select(TeamId) %>%
  left_join(teams) %>%
  group_by(CompetitionId) %>%
  count(TeamId) %>%
  rename(Submissions = n) %>%
  ungroup

max_scores <- tbl(db, "Submissions") %>%
  select(TeamId, PublicScore) %>%
  left_join(teams) %>%
  group_by(CompetitionId, TeamId) %>%
  summarize(Score = max(PublicScore)) %>%
  ungroup

leaderboards <- left_join(n_submissions, max_scores) %>%
  left_join(team_sizes) %>%
  as_data_frame() %>%
  na.omit()

# Convert to format used in original reports
leaderboards %<>% rename(
  slug = CompetitionId,
  score = Score,
  team_size = TeamSize,
  entries = Submissions
)

# Enumerate submissions per team

submissions <- tbl(db, "Submissions") %>%
  as_data_frame() %>%
  group_by(TeamId) %>%
  arrange(DateSubmitted) %>%
  mutate(SubmissionNum = 1:n()) %>%
  ungroup() %>%
  select(TeamId, Score = PublicScore, SubmissionNum)


use_data(
  leaderboards,
  overwrite = TRUE
)
