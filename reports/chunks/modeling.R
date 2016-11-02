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
ellipse_x <- function(t, x0, a) x0 + a * cos(t)
ellipse_y <- function(t, y0, b) y0 + b * sin(t)

search_areas <- list(
  overlapping = function(player) {
    data_frame(
      player = player,
      team_ability = "identical",
      a = ellipse_x(t, 0.5, 10),
      b = ellipse_y(t, 0.5, 10)
    )
  },
  disjoint = function(player) {
    args <- list(
      x = list(x0 = 0.5, y0 = 0.5, a = 20, b = 5),
      y = list(x0 = 0.5, y0 = 0.5, a = 5, b = 20)
    )[[player]]
    data_frame(
      player = player,
      team_ability = "disjoint",
      a = ellipse_x(t, args[["x0"]], args[["a"]]),
      b = ellipse_y(t, args[["y0"]], args[["b"]])
    )
  },
  intersecting = function(player) {
    data_frame(
      player = player,
      team_ability = "intersecting",
      a = ellipse_x(t, 0.5, 10),
      b = ellipse_y(t, 0.5, 10)
    )
  }
)

spotlight_coords <- lapply(search_areas, function(area_fn) {
    # generate areas for players x and y
    bind_rows(area_fn('x'), area_fn('y'))
  }) %>%
  # bind across area_fns
  bind_rows()

ggplot(spotlight_coords, aes(a, b, group = player)) +
  geom_line() +
  facet_wrap("team_ability")
