#' Recode team structures
#' @import dplyr
#' @export
recode_team_structures <- function(frame) {
  team_structures <- c("synchronic", "diachronic")
  team_structures_map <- data_frame(
    team_structure = team_structures,
    team_factor = factor(team_structures, levels = team_structures,
                         labels = str_to_title(team_structures))
  )
  left_join(frame, team_structures_map)
}

#' Recode feedback types
#' @import dplyr
#' @import stringr
#' @export
recode_feedback_types <- function(frame) {
  feedback_types <- c("yes", "no", "enforce")
  feedback_map <- data_frame(
    feedback_type = feedback_types,
    feedback_factor = factor(feedback_type, levels = feedback_types,
                             labels = str_to_title(feedback_types))
  )
  left_join(frame, feedback_map)
}
