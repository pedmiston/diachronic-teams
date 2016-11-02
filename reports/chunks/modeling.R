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

base_theme <- theme(axis.ticks = element_blank(),
                    legend.position = "none",
                    plot.title = element_text(hjust = 0))

# ---- team-abilities
preds <- expand.grid(
  skill = skills,
  player = players,
  team_ability = team_abilities,
  stringsAsFactors = FALSE
) %>% select(team_ability, player, skill)
# > preds
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

legend_text <- data_frame(
  team_ability = "identical",
  player = "x",
  value = c(0.1, 0.1),
  skill = c("a", "b"),
  label = c("skill a", "skill b")
) %>% recode_team_abilities

dodge_width <- position_dodge(width = 0.9)
ggplot(preds, aes(x = player, y = value, group = skill)) +
  geom_bar(aes(fill = team_ability, alpha = skill),
           stat = "identity", position = dodge_width) +
  facet_wrap("team_ability_label", nrow = 1) +
  scale_fill_brewer(palette = "Set2") +
  scale_alpha_manual(values = c(0.5, 0.9)) +
  geom_text(aes(label = label), data = legend_text, position = dodge_width,
            angle = 90, vjust = 0.5, hjust = 0) +
  base_theme +
  labs(title = "Types of two person teams varying in skill distribution")

# ---- search-areas
t_range <- seq(0, 2 * pi, length.out = 100)
ellipse_x <- function(t, a, x0 = 0) x0 + a * cos(t)
ellipse_y <- function(t, b, y0 = 0) y0 + b * sin(t)

# Create a list of different area functions.
# Each fn returns data.frames with the same col names.
search_areas <- list(
  overlapping = function(player) {
    data_frame(
      player = player,
      team_ability = "identical",
      a = ellipse_x(t_range, 10),
      b = ellipse_y(t_range, 10)
    )
  },
  disjoint = function(player) {
    args <- list(
      x = list(a = 17, b = 3),
      y = list(a = 3, b = 17)
    )[[player]]
    data_frame(
      player = player,
      team_ability = "disjoint",
      a = ellipse_x(t_range, args[["a"]]),
      b = ellipse_y(t_range, args[["b"]])
    )
  },
  intersecting = function(player) {
    args <- list(
      x = list(a = 12, b = 8),
      y = list(a = 8, b = 12)
    )[[player]]
    data_frame(
      player = player,
      team_ability = "intersecting",
      a = ellipse_x(t_range, args[["a"]]),
      b = ellipse_y(t_range, args[["b"]])
    )
  }
)

spotlight_coords <- lapply(search_areas, function(area_fn) {
    # generate areas for players x and y and bind them together
    bind_rows(area_fn('x'), area_fn('y'))
  }) %>%
  # bind across area_fns
  bind_rows() %>%
  recode_team_abilities

legend_text <- data_frame(
  team_ability = "disjoint",
  player = c("x", "y"),
  a = c(10, 0),
  b = c(0, 10),
  label = c("player x", "player y"),
  angle = c(0, 90)
) %>%
  recode_team_abilities

ggplot(spotlight_coords, aes(a, b, group = player)) +
  geom_polygon(aes(fill = team_ability, alpha = player)) +
  facet_wrap("team_ability_label") +
  scale_fill_brewer(palette = "Set2") +
  scale_alpha_manual(values = c(0.5, 0.9)) +
  geom_text(aes(label = label, angle = angle), data = legend_text,
            vjust = 0.5) +
  base_theme +
  labs(title = "Abilities of two person teams as search areas in problem space")
