#' Recode strategy condition by merging with a map of labels.
#'
#' If no frame is provided, just the map of labels is returned.
#'
#' @import dplyr
#' @import magrittr
#' @export
recode_strategy <- function(frame) {
  strategies <- c("Synchronic", "Isolated", "Diachronic")
  map <- data_frame(Strategy = strategies,
                    StrategyLabel = factor(strategies, levels = strategies))

  # Add treatment contrasts
  treatment_contrasts <- contr.treatment(strategies, base = 3) %>%
    as.data.frame() %>%
    rename(Diachronic_v_Synchronic = Synchronic, Diachronic_v_Isolated = Isolated) %>%
    mutate(Strategy = row.names(.))
  map %<>% left_join(treatment_contrasts)

  if (missing(frame)) return(map)
  left_join(frame, map)
}


#' Recode strategy and group by generation.
#'
#' Only matters for Diachronic teams. Aides in plotting over time.
#'
#' @import dplyr
#' @export
recode_groups_by_generation <- function(frame) {
  frame %>%
    mutate(
      ID_Group_Time = ifelse(Strategy == "Diachronic",
          paste(ID_Group, Generation, sep = "-"), ID_Group),
      Strategy_Group_Time = ifelse(Strategy == "Diachronic",
          paste(Strategy, Generation, sep = "-"), Strategy)
    )
}


#' Recode the labels of the type of scoring metric.
#' @import dplyr
#' @export
recode_score_value <- function(frame) {
  measures <- c("Score", "InventorySize", "DifficultyScore")
  labels <- c("Totem score", "Inventory size", "Difficulty score")
  score_value_map <- data_frame(
    Measure = measures,
    MeasureLabel = factor(measures, levels = measures, labels = labels)
  )
  if (missing(frame)) return(score_value_map)
  left_join(frame, score_value_map)
}


#' Recode the labels of the type of attempt measure.
#' @import dplyr
#' @export
recode_attempt_measures <- function(frame) {
  attempt_measures <- c("Attempts", "TeamAttempts")
  labels <- c("Individual attempts", "Team attempts")
  attempt_measures_map <- data_frame(
    AttemptMeasure = attempt_measures,
    AttemptMeasureLabel = factor(attempt_measures, levels = attempt_measures, labels = labels)
  )
  if (missing(frame)) return(attempt_measures_map)
  left_join(frame, attempt_measures_map)
}
