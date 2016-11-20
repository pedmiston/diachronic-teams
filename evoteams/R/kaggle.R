
#' Connect to the Kaggle SQLite database.
#'
#' @param sqlite_location Location of Kaggle sqlite database. Download this file
#'   from \url{https://www.kaggle.com/kaggle/meta-kaggle/downloads/database.sqlite.zip},
#'   unzip it, and put it wherever you'd like. By default, the file is expected
#'   to be found in the "evoteams" package, in the "extdata/" directory and
#'   named "kaggle.sqlite".
#'
#' @export
connect_kaggle <- function(sqlite_location) {
  if (missing(sqlite_location)) {
    sqlite_location <- system.file("extdata/kaggle.sqlite", package = "evoteams")
  }
  if (!file.exists(sqlite_location)) stop("kaggle.sqlite not found")

  dplyr::src_sqlite(sqlite_location)
}


#' Label predicted place.
#' @export
predict_place <- function(submissions, leaderboards) {
  determine_breaks <- function(competition_id) {
    leaderboards %>%
      filter(CompetitionId == competition_id) %>%
      .$Score %>%
      unique %>%
      c(-Inf, ., Inf)
  }

  submissions %>%
    group_by(CompetitionId) %>%
    mutate(
      PredictedPlace = cut(rev(Score), breaks=determine_breaks(CompetitionId[[1]]), labels=FALSE)
    )
}


#' Compute time interval between first and last submissions.
#' @export
time_interval <- function(submissions) {
  submissions %>%
    group_by(TeamId) %>%
    summarize(
      FirstSubmissionTime = min(DateSubmitted),
      LastSubmissionTime = max(DateSubmitted)
    ) %>%
    mutate(
      TotalTime = as.duration(interval(FirstSubmissionTime, LastSubmissionTime)),
      TotalTimeSec = as.numeric(TotalTime)
    )
}

#' Cut total submissions into bins and label with the middle value.
#' @import dplyr
#' @export
label_submission_bins <- function(frame, submission_bin_width = 10) {
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



#' Create a ggplot2 scale object for time variable in seconds.
#' @export
make_time_scale <- function(name, breaks_days,
                            scale_obj = "scale_y_continuous", ...) {
  breaks_sec <- breaks_days * 24 * 3600
  get(scale_obj)(name, breaks = breaks_sec, labels = breaks_days, ...)
}
