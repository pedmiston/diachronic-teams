find_figure <- function(name, ext) {
  figure_file <- paste0(name, ext)
  figure_path <- system.file("extdata", figure_file, package = "evoteams")
  if (!file.exists(figure_path)) stop("File does not exist")
  figure_path
}

#' Returns a path to a graphviz file included in this package
#' @export
find_graphviz <- function(name) {
  find_figure(name, ext = ".gv")
}

#' @importFrom knitr read_chunk
#' @export
read_all_graphviz_chunks <- function() {
  graphviz_chunks <- c("team-structure-single-project", "team-structure-multiple-projects")
  for (chunk in graphviz_chunks) {
    chunk_path <- find_graphviz(chunk)
    read_chunk(chunk_path, labels = chunk)
  }
}
