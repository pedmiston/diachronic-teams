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

