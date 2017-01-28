library(magrittr)
library(tools)
library(readr)

#' Strip the stem name off a file path.
#'
#' @example file_stem("path/to/my/file.ext") == "file"
#'
#' @import magrittr
#' @import tools
file_stem <- function(x) {
  basename(x) %>%
    file_path_sans_ext() %>%
    tolower()
}

#' Util function for loading a directory of csvs
#' and assigning the resulting data frames with
#' prefixed names.
#'
#' @import readr
assign_csvs_with_prefix <- function(directory, prefix) {
  for (csv in list.files(directory, pattern = "*.csv", full.name = TRUE)) {
    stem <- file_stem(csv)
    assign(paste0(prefix, stem), read_csv(csv), envir = globalenv())
  }
}
