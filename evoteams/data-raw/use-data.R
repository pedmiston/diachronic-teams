library(devtools)
library(tidyverse)
library(tools)

file_stem <- function(x) x %>% basename() %>% file_path_sans_ext()

bots_files <- list.files("data-raw/bots", pattern = "*.csv", full.names = TRUE)

for (exp in bots_files) {
  stem <- file_stem(exp)
  assign(paste0("bots_", stem), read_csv(exp))
}

use_data(
  bots_main,
  bots_replication,
  overwrite = TRUE
)
