#' @export
z_score <- function(x) (x - mean(x))/sd(x)

#' Strip the stem name off a file path.
#'
#' @import magrittr
#' @import tools
file_stem <- function(x) {
  basename(x) %>%
    tools::file_path_sans_ext()
}

#' Util function for loading a directory of csvs
#' and assigning the resulting data frames with
#' prefixed names.
#'
#' @import readr
read_csvs <- function(directory, prefix = "", stem_func = function(x) x) {
  for (csv in list.files(directory, pattern = "*.csv", full.name = TRUE)) {
    stem <- file_stem(csv) %>% stem_func()
    assign(paste0(prefix, stem), readr::read_csv(csv), envir = globalenv())
  }
}
