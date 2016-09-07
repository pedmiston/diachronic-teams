find_figure <- function(name, ext) {
  figure_file <- paste0(name, ext)
  system.file("extdata", figure_file, package = "evoteams")
}

#' Returns a path to a graphviz file included in this package
#' @export
find_graphviz <- function(name) {
  find_figure(name, ext = ".gv")
}
