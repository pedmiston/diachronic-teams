
#' Return a list containing theme elements to be used across visualizations.
#' @export
load_totems_theme <- function() {
  strategy_levels = levels(recode_strategy()$StrategyLabel)
  strategy_colors = RColorBrewer::brewer.pal(length(strategy_levels), "Set2")

  totems_theme <- list(
    # Theme
    base_theme = ggplot2::theme_minimal(),
    # Colors
    synchronic_color = strategy_colors[1],
    isolated_color = strategy_colors[2],
    diachronic_color = strategy_colors[3],
    # Strategy scales
    scale_x_strategy = ggplot2::scale_x_discrete("Strategy", labels = strategy_levels),
    scale_color_strategy = ggplot2::scale_color_manual("Strategy", values = strategy_colors),
    scale_fill_strategy = ggplot2::scale_fill_manual("Strategy", values = strategy_colors)
  )

  totems_theme
}
