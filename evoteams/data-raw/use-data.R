library(tools)
library(dplyr)
library(readr)
library(devtools)

data_files <- list.files("data-raw", pattern = "*.csv",
                         full.names = TRUE, recursive = TRUE)
stem <- function(path) file_path_sans_ext(basename(path))

for (path in data_files) {
  frame <- read_csv(path)
  name <- stem(path)
  assign(name, frame)
}

use_data(
  competitions,
  leaderboards,
  overwrite = TRUE
)
