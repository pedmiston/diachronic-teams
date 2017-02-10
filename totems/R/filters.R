#' Select only the rows containing the team's highest score.
#'
#' @import dplyr
#' @export
filter_final_score <- function(frame) {
  frame %>%
    group_by(Strategy, ID_Group) %>%
    summarize(Score = max(Score)) %>%
    recode_strategy()
}
