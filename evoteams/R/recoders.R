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


#' Recode team type for labels and contrast coding.
#'
#' If no data is provided, the team type map is returned.
#'
#' @import dplyr
#' @export
recode_team_type <- function(frame) {
  team_type_levels <- c("steady", "long", "short", "rapid")

  contr_rel_short <- contr.treatment(n = length(team_type_levels), base = 3) %>%
    as_data_frame()
  names(contr_rel_short) <- c("ShortVSteady", "ShortVLong", "ShortVRapid")
  contr_rel_short$TeamType <- team_type_levels

  team_type_map <- data_frame(
    TeamType = team_type_levels,
    TeamLabel = factor(team_type_levels, levels = team_type_levels),
    TeamNum = seq_along(team_type_levels)
  ) %>%
    left_join(contr_rel_short)

  if (missing(frame)) return(team_type_map)

  left_join(frame, team_type_map)
}
