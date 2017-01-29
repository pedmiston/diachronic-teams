# ---- setup
library(tidyverse)
library(magrittr)
library(grid)
library(gridExtra)
library(broom)
library(crotchet)

library(evoteams)
data("totems_players")
data("totems_workshops")

base_theme <- theme_minimal()

# Strategy
strategy_levels <- levels(recode_strategy()$StrategyLabel)
strategy_colors <- RColorBrewer::brewer.pal(3, "Set2")
scale_x_strategy <- scale_x_discrete("Strategy", labels = strategy_levels)
scale_color_strategy <- scale_color_manual(values = strategy_colors)
scale_fill_strategy <- scale_fill_manual(values = strategy_colors)
