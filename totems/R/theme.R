
#' Return a list containing theme elements to be used across visualizations.
#' @export
load_totems_theme <- function() {
  strategy_levels = levels(recode_strategy()$StrategyLabel)
  strategy_colors = RColorBrewer::brewer.pal(3, "Set2")

  totems_theme <- list(
    base_theme = ggplot2::theme_minimal(),
    # Strategy
    scale_x_strategy = ggplot2::scale_x_discrete("Strategy", labels = strategy_levels),
    scale_color_strategy = ggplot2::scale_color_manual(values = strategy_colors),
    scale_fill_strategy = ggplot2::scale_fill_manual(values = strategy_colors)
  )

  totems_theme
}
