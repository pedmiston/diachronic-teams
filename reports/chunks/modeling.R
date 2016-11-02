# ---- setup
library(magrittr)
library(dplyr)
library(ggplot2)
library(grid)

team_structures <- c("diachronic", "sychronic")
team_abilities <- c("identical", "disjoint", "intersecting")
skills <- c("a", "b")
players <- c("x", "y")

recode_team_abilities <- function(frame) {
  data_frame(
    team_ability = team_abilities,
    team_ability_label = factor(team_abilities, levels = team_abilities)
  ) %>%
    left_join(frame, .)
}

# ---- team-abilities
preds <- expand.grid(
  skill = skills,
  player = players,
  team_ability = team_abilities,
  stringsAsFactors = FALSE
) %>% select(team_ability, player, skill)
preds
#    team_ability player skill
# 1     identical      x     a
# 2     identical      x     b
# 3     identical      y     a
# 4     identical      y     b
# 5      disjoint      x     a
# 6      disjoint      x     b
# 7      disjoint      y     a
# 8      disjoint      y     b
# 9  intersecting      x     a
# 10 intersecting      x     b
# 11 intersecting      y     a
# 12 intersecting      y     b
preds$value <- c(5, 5, 5, 5,
                 2, 8, 8, 2,
                 4, 6, 6, 4)

preds %<>%
  recode_team_abilities()

ggplot(preds, aes(x = player, y = value)) +
  geom_bar(aes(group = skill, fill = team_ability, alpha = skill),
           stat = "identity", position = "dodge") +
  facet_wrap("team_ability_label", nrow = 1) +
  scale_fill_brewer(palette = "Set2") +
  scale_alpha_manual(values = c(0.5, 0.9))

# ---- search-areas
margin <- 0.1
ellipse_points <- function(x0, y0, a, b) {
  ellipse_x <- function(t) x0 + a * cos(t)
  ellipse_y <- function(t) y0 + b * sin(t)
  t <- seq(0, 2 * pi, length.out = 100)
  data_frame(
    a = ellipse_x(t),
    b = ellipse_y(t)
  )
}

x_axis <- linesGrob(x = c(margin, 1 - margin), y = c(margin, margin), arrow = arrow())
y_axis <- linesGrob(x = c(margin, margin), y = c(margin, 1 - margin), arrow = arrow())

circle <- circleGrob(r = 0.25, x = 0.5 + margin/2, y = 0.5 + margin/2)

identical <- gTree(x_axis, y_axis, circle)



plot_axes()

v1 <- 0.3
v2 <- 0.1
player_x <- ellipse_points(0.5 + margin, 0.25, v1, v2)
player_y <- ellipse_points(0.25, 0.5 + margin, v2, v1)

grid.newpage()
plot_axes()
grid.polyline(x = player_x$a, y = player_x$b)
grid.polyline(x = player_y$a, y = player_y$b)

v1 <- 0.25
v2 <- 0.15
sep <- 0.05
player_x <- ellipse_points(0.5 + sep, 0.5 - sep, v1, v2)
player_y <- ellipse_points(0.5 - sep, 0.5 + sep, v2, v1)

grid.newpage()
plot_axes()
grid.polyline(x = player_x$a, y = player_x$b)
grid.polyline(x = player_y$a, y = player_y$b)
