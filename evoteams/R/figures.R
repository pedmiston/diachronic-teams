#' @importFrom knitr read_chunk
#' @importFrom crotchet find_graphviz
#' @export
read_evoteams_chunks <- function() {
  graphviz_chunks <- c("team-structure-single-project", "team-structure-multiple-projects")
  for (chunk in graphviz_chunks) {
    chunk_path <- find_graphviz(chunk, package = "evoteams")
    read_chunk(chunk_path, labels = chunk)
  }
}
