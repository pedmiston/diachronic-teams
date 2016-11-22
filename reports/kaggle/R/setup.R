# ---- setup
library(dplyr)
library(magrittr)

library(ggplot2)
library(gridExtra)
library(scales)

library(lme4)
library(AICcmodavg)

library(evoteams)

kaggle_db <- connect_kaggle()
submissions <- get_submissions(kaggle_db)
leaderboards <- make_leaderboards(kaggle_db, submissions)

# Investigate top 100 places only
top_100 <- leaderboards %>%
  filter(Place <= 100)

# Summarize team properties in each place.
top_100_places <- summarize_by_place(top_100)

# Summarize performance in groups:
top_100_by_team_size <- top_100 %>%
  group_by(TeamSize) %>%
  summarize_teams_in_group()

# By submission bin
top_100_by_submission_bin <- top_100 %>%
  label_submission_bins() %>%
  group_by(TotalSubmissionsBin) %>%
  summarize_teams_in_group()

# By relative submission bin
# top_100_by_rel_submission_bin <- top_100 %>%
#   label_submission_bins() %>%
#   group_by(RelativeSubmissionsBin) %>%
#   summarize_teams_in_group()