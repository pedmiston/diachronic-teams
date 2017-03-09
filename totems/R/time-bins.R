#' @import dplyr
#' @export
create_time_bins <- function(frame, bin_duration = 5) {
  frame %>%
    mutate(
      PlayerTimeBin = cut_times(PlayerTime, bin_duration = bin_duration) * bin_duration,
      TeamTimeBin   = cut_times(TeamTime, bin_duration = bin_duration) * bin_duration
    )
}


#' @import dplyr
#' @export
cut_times <- function(times, bin_duration = 5) {
  breaks <- seq(0, max(times) + bin_duration, by = bin_duration)
  cut(times, breaks = breaks, right = FALSE) %>%
    as.numeric() - 1
}


#' @export
get_trials_at_times <- function(trials, times, bin_duration = 5, time_col = "TeamTime") {
  lapply(times, get_closest_trial_to_time, trials = trials, bin_duration = bin_duration) %>%
    bind_rows(.id = paste0(time_col, "Exact")) %>%
    left_join(trials)
}


#' @export
get_closest_trial_to_time <- function(time, trials, time_col = "TeamTime", bin_duration = 5) {
  needle <- (trials[[time_col]] > (time - bin_duration)) & (trials[[time_col]] <= time)
  trials[needle, ]
}
