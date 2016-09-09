#' Read all available knitr chunks in the evoteams package.
#' @importFrom crotchet read_all_graphviz_chunks
#' @export
read_evoteams_chunks <- function() {
  read_all_graphviz_chunks("evoteams")
}
