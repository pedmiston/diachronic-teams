
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
      c(-Inf, ., Inf)
  }

  submissions %>%
    group_by(CompetitionId) %>%
    mutate(
      PredictedPlace = cut(rev(Score), breaks=determine_breaks(CompetitionId[[1]]), labels=FALSE)
    )
}
