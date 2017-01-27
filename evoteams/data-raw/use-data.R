library(devtools)
library(tidyverse)
library(magrittr)
library(tools)

file_stem <- function(x) x %>% basename() %>% file_path_sans_ext() %>% tolower()

assign_csvs_with_prefix <- function(directory, prefix) {
  for (csv in list.files(directory, pattern = "*.csv", full.name = TRUE)) {
    stem <- file_stem(csv)
    assign(paste0(prefix, stem), read_csv(csv), envir = globalenv())
  }
}

try({
  print("----- Trying to load simulation results")
  assign_csvs_with_prefix("data-raw/bots", "bots_")
  use_data(
    bots_replication,
    bots_strategy,
    bots_guesses,
    bots_players,
    bots_memory,
    overwrite = TRUE
  )
  print("----- Succeeded")
})

try({
  print("----- Trying to load totems experiment results")
  assign_csvs_with_prefix("data-raw/totems", "totems_")

  totems_leaderboards <- totems_player %>%
    left_join(totems_group) %>%
    rename(Strategy = Treatment) %>%
    inner_join(totems_subjinfo)

  use_data(
    totems_leaderboards,
    overwrite = TRUE
  )
  print("----- Succeeded")
})
