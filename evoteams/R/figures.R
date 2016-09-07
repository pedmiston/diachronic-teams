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
