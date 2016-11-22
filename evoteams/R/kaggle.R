#' Connect to the Kaggle SQLite database.
#'
#' @param sqlite_location Location of Kaggle sqlite database. Download this file
#'   from \url{https://www.kaggle.com/kaggle/meta-kaggle/downloads/database.sqlite.zip},
#'   unzip it, and put it wherever you'd like. By default, the file is expected
#'   to be found in the "evoteams" package at "inst/extdata/kaggle.sqlite".
#'
#' @export
connect_kaggle <- function(sqlite_location) {
  if (missing(sqlite_location)) {
    sqlite_location <- system.file("extdata/kaggle.sqlite", package = "evoteams")
  }
  if (!file.exists(sqlite_location)) stop("kaggle.sqlite not found")

  dplyr::src_sqlite(sqlite_location)
}


#' Get all submissions to all Kaggle competitions.
#'
#' Simplifies the Submissions table of the Kaggle db.
#'
#' @param team_competitions Used to label competition ids in submissions.
#'    If not provided, team competitions will be generated from the provided
#'    Kaggle db.
#' @param with_predicted_place Set to true and/or provide leaderboards to
#'    augment submissions with a PredictedPlace column based on final
#'    standings.
#'
#' @import dplyr
#' @import magrittr
#' @export
get_submissions <- function(kaggle_db, team_competitions,
                            with_predicted_place = TRUE) {
  if (missing(kaggle_db)) stop("must provide connection to kaggle db")

  if (missing(team_competitions)) {
    team_competitions <- get_team_competitions(kaggle_db)
  }

  submissions <- kaggle_db %>%
    tbl("Submissions") %>%
    select(TeamId, DateSubmitted, Score = PublicScore) %>%
    # Break DBI to allow normal data frame manipulation
    as_data_frame() %>%
    mutate(DateSubmitted = lubridate::ymd_hms(DateSubmitted)) %>%
    group_by(TeamId) %>%
    arrange(DateSubmitted) %>%
    mutate(SubmissionNum = 1:n()) %>%
    ungroup() %>%
    # Label competition id
    left_join(team_competitions)

  if (with_predicted_place) {
    leaderboards <- make_leaderboards(kaggle_db, submissions,
                                      with_submission_intervals = FALSE,
                                      with_competition_intervals = FALSE)
    submissions %<>% predict_place(leaderboards)
  }

  submissions
}


#' Create Kaggle leaderboards from submissions.
#'
#' Provide either submissions or a connection to the Kaggle db.
#'
#' @examples
#' submissions <- get_submissions(kaggle_db)
#' leaderboards <- make_leaderboards(submissions)
#' leaderboards <- make_leaderboards(kaggle_db = kaggle_db)
#'
#' @param kaggle_db sqlite connector. Required unless submissions and team sizes
#'    are both provided.
#' @param submissions data frame of Kaggle submissions. If submissions aren't
#'    provided, they will be created.
#' @param team_sizes data frame map of Kaggle team ids to team sizes. Required
#'    if connection to Kaggle db is not provided.
#' @param with_submission_intervals Logical. Should submission intervals
#'    be calculated? Defaults to true. Calls
#'    \code{\link{calculate_submission_intervals}} to create the intervals.
#'
#' @import dplyr
#' @import magrittr
#' @export
make_leaderboards <- function(kaggle_db, submissions, team_sizes,
                              with_submission_intervals = TRUE,
                              with_competition_intervals = TRUE) {
  if (missing(kaggle_db) & any(missing(submissions), missing(team_sizes))) {
    stop("must provide kaggle db or all required data frames")
  }

  # Only create the data frames that weren't provided
  if (missing(submissions)) submissions <- get_submissions(kaggle_db)
  if (missing(team_sizes)) team_sizes <- get_team_sizes(kaggle_db)

  leaderboards <- submissions %>%
    # Select the final submission for each team
    group_by(TeamId) %>%
    filter(SubmissionNum == max(SubmissionNum)) %>%
    rename(TotalSubmissions = SubmissionNum) %>%
    ungroup() %>%
    # Assign places for each competition
    group_by(CompetitionId) %>%
    arrange(desc(Score)) %>%  # assumes bigger scores are better
    mutate(Place = 1:n()) %>%
    ungroup() %>%
    left_join(team_sizes) %>%
    as_data_frame()

  if (with_submission_intervals) {
    submission_intervals <- calculate_submission_intervals(submissions)
    leaderboards %<>% left_join(submission_intervals)
  }

  if (with_competition_intervals) {
    if (missing(kaggle_db)) stop("kaggle db required to get competition intervals")
    competition_intervals <- get_competition_intervals(kaggle_db)
    leaderboards %<>%
      left_join(competition_intervals) %>%
      mutate(PropCompetitionTime = TotalTimeSec/CompetitionDurationSec)
  }

  leaderboards
}


#' Create a map of team ids to competition ids.
#' @import dplyr
#' @export
get_team_competitions <- function(kaggle_db) {
  kaggle_db %>%
    tbl("Teams") %>%
    select(TeamId = Id, CompetitionId) %>%
    as_data_frame()
}


#' Calculate Kaggle team sizes.
#' @import dplyr
#' @export
get_team_sizes <- function(kaggle_db) {
  kaggle_db %>%
    tbl("TeamMemberships") %>%
    select(TeamId, UserId) %>%
    count(TeamId) %>%
    rename(TeamSize = n) %>%
    as_data_frame()
}


#' Get competition start and end times.
#' @import dplyr
#' @export
get_competition_intervals <- function(kaggle_db) {
  kaggle_db %>%
    tbl("Competitions") %>%
    as_data_frame() %>%
    mutate(
      CompetitionStart = lubridate::ymd_hms(DateEnabled),
      CompetitionEnd = lubridate::ymd_hms(Deadline),
      CompetitionDuration = interval_duration(CompetitionStart, CompetitionEnd),
      CompetitionDurationSec = as.numeric(CompetitionDuration)
    ) %>%
    select(CompetitionId = Id, CompetitionDuration, CompetitionDurationSec)
}


#' Label predicted place.
#' @import dplyr
#' @export
predict_place <- function(submissions, leaderboards) {
  determine_breaks <- function(competition_id) {
    leaderboards %>%
      filter(CompetitionId == competition_id) %>%
      .$Score %>%
      unique %>%
      c(-Inf, ., Inf)
  }

  predict_place <- function(scores, competition_id) {
    cut(rev(scores), breaks = determine_breaks(competition_id), labels = FALSE)
  }

  submissions %>%
    group_by(CompetitionId) %>%
    mutate(PredictedPlace = predict_place(Score, CompetitionId[[1]]))
}


#' Compute time interval between first and last submissions.
#' @import dplyr
#' @export
calculate_submission_intervals <- function(submissions) {
  submissions %>%
    group_by(TeamId) %>%
    summarize(
      FirstSubmissionTime = min(DateSubmitted),
      LastSubmissionTime = max(DateSubmitted)
    ) %>%
    mutate(
      TotalTime = interval_duration(FirstSubmissionTime, LastSubmissionTime),
      TotalTimeSec = as.numeric(TotalTime)
    )
}


#' Cut total submissions into bins and label with the middle value.
#' @import dplyr
#' @export
label_submission_bins <- function(leaderboard, submission_bin_width = 10) {
  breaks <- seq(1, max(leaderboard$TotalSubmissions) + 1, by = submission_bin_width)
  labels <- cbind(break_min = breaks, break_max = lead(breaks, n = 1) - 1) %>%
    as_data_frame %>%
    head(-1) %>%  # drop last row containing open interval
    mutate(break_means = rowMeans(.[, c("break_min", "break_max")])) %>%
    .$break_means

  bins <- cut(leaderboard$TotalSubmissions, breaks = breaks, labels = labels,
              right = FALSE)
  bins <- as.numeric(as.character(bins))  # factor -> character -> numeric

  leaderboard %>%
    mutate(TotalSubmissionsBin = bins)
}


#' Create a ggplot2 scale object for time variable in seconds.
#' @import ggplot2
#' @export
make_time_scale <- function(name, breaks_days,
                            scale_obj = "scale_y_continuous", ...) {
  breaks_sec <- breaks_days * 24 * 3600
  get(scale_obj)(name, breaks = breaks_sec, labels = breaks_days, ...)
}


#' Label different place groups, i.e., FirstPlaceTeam.
#' @export
label_place_groups <- function(frame) {
  dplyr::mutate(frame, FirstPlaceTeam = (Place == 1))
}


#' Summarize team properties in each place.
#'
#' Each row is a place. For example, the row for Place == 1
#' describes the average number of TotalSubmissions and average
#' TeamSize for all teams that finished in this place.
#'
#' @examples
#' leaderboards %>% filter(Place <= 100) %>% summarize_by_place()
#'
#' @import dplyr
#' @export
summarize_by_place <- function(leaderboards) {
  leaderboards %>%
    group_by(Place) %>%
    summarize(
      TotalSubmissions = mean(TotalSubmissions),
      TeamSize = mean(TeamSize, na.rm = TRUE),
      TotalTime = mean(TotalTime),
      PropCompetitionTime = mean(PropCompetitionTime, na.rm = TRUE)
    ) %>%
    label_place_groups()
}


#' Summarize team performance in a group.
#'
#' This functional sequence is meant to be passed a grouped data frame.
#'
#' @examples
#' leaderboards %>% group_by(TeamSize) %>% summarize_teams_in_group()
#'
#' @import dplyr
#' @export
summarize_teams_in_group <- function(grouped) {
  grouped %>%
    summarize(
      Place = mean(Place),
      TotalTime = mean(TotalTime),
      PropCompetitionTime = mean(PropCompetitionTime),
      NTeams = n()
    ) %>%
    mutate(
      PercentTeams = NTeams/sum(NTeams),
      PercentTeamsLabel = scales::percent(PercentTeams)
    )
}


#' Get model predictions for a model predicting Place from TotalSubmissions.
#'
#' @import dplyr
#' @export
get_place_mod_preds <- function(mod, predict_fn, x_preds) {
  if (missing(x_preds)) x_preds <- data_frame(TotalSubmissions = 1:200)
  if (missing(predict_fn)) predict_fn <- predict

  x_preds %>%
    cbind(., predict_fn(mod, ., se = TRUE)) %>%
    rename(Place = fit, SE = se.fit) %>%
    mutate(TotalSubmissionsBin = TotalSubmissions)  # for consistency with summary
}


interval_duration <- function(start, end) {
  lubridate::interval(start, end) %>% lubridate::as.duration()
}
