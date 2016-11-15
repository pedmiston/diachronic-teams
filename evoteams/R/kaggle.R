
#' Connect to the Kaggle SQLite database.
#' @export
connect_kaggle <- function(sqlite_location) {
  if (missing(sqlite_location)) {
    sqlite_location <- system.file("kaggle.sqlite", package = "evoteams")
  }
  if (!file.exists(sqlite_location)) stop("kaggle.sqlite not found")

  dplyr::src_sqlite(sqlite_location)
}
