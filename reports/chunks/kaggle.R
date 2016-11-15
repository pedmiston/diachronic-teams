# ---- setup
library(dplyr)
library(magrittr)

library(ggplot2)
library(gridExtra)
library(scales)

library(lme4)
library(AICcmodavg)

# data
library(evoteams)
data("leaderboards")

leaderboards %<>% mutate(team_size = as.numeric(team_size))

leaderboards %<>%
  group_by(slug) %>%
  arrange(desc(score)) %>%
  mutate(place = 1:n()) %>%
  ungroup

top_100 <- leaderboards %>%
  filter(place <= 100) %>%
  mutate(first_place = place == 1)

summarize_by_place <- function(frame) {
  frame %>%
    summarize(
      place = mean(place),
      n = n()
    ) %>%
    mutate(
      pct = n/sum(n),
      pct_label = percent(pct)
    )
}

by_place <- top_100 %>%
  group_by(first_place, place) %>%
  summarize(
    entries = mean(entries),
    team_size = mean(team_size)
  )

by_team_size <- top_100 %>%
  group_by(team_size) %>%
  summarize_by_place

by_submissions <- top_100 %>%
  group_by(entries) %>%
  summarize_by_place

# theme
base_theme <- theme_minimal() +
  theme(
    legend.position = "none",
    axis.ticks = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(size = 12)
  )

colors <- RColorBrewer::brewer.pal(3, "Set2")
names(colors) <- c("green", "orange", "blue")
colors["first_place"] <- colors[["orange"]]
colors["submissions"] <- colors[["blue"]]
colors["team_size"] <- colors[["green"]]

default_alpha <- 0.6

# scales
scale_x_place <- scale_x_continuous(breaks = c(1, seq(10, 100, by = 10)))
scale_x_team_size <- scale_x_continuous("team size", breaks = 1:4)
scale_y_team_size <- scale_y_continuous("team size", breaks = 1:4)
scale_y_submissions <- scale_y_continuous("submissions", breaks = c(1, seq(5, 100, by = 5)))
scale_y_place <- scale_y_reverse("place", breaks = c(1, seq(10, 100, by = 10)))

# ---- submissions-from-place
ggplot(by_place, aes(place, entries)) +
  geom_point(aes(color = first_place), alpha = default_alpha) +
  scale_x_place +
  scale_y_submissions +
  scale_color_manual(values = c(colors[["submissions"]], colors[["first_place"]])) +
  coord_cartesian(xlim = c(1, 100), ylim = c(1, 39)) +
  base_theme +
  ggtitle("Top place teams make more submissions")

# ---- place-from-submissions
gg_place_from_submissions <- ggplot(by_submissions, aes(entries, place)) +
  geom_point(aes(size = pct), alpha = default_alpha, color = colors[["submissions"]]) +
  scale_x_continuous("submissions", breaks = c(1, seq(100, 600, by = 100))) +
  scale_y_place +
  scale_size_continuous("Teams", labels = percent) +
  coord_cartesian(xlim = c(1, 600), ylim = c(1, 100)) +
  base_theme +
  theme(
    legend.position = "bottom"
  ) +
  ggtitle("Making more submissions improves place")

place_mod <- lm(place ~ entries + team_size, data = top_100)

x_preds <- expand.grid(entries = 1:200, team_size = 1)
y_preds <- predict(place_mod, x_preds, se = TRUE)
preds <- cbind(x_preds, y_preds) %>%
  rename(place = fit, se = se.fit)

gg_place_from_submissions +
  geom_smooth(aes(ymin = place - se, ymax = place + se), data = preds,
              stat = "identity", color = colors[["orange"]])

# ---- teamsize-from-place
ggplot(by_place, aes(place, team_size)) +
  geom_point(aes(color = first_place), alpha = default_alpha) +
  scale_x_place +
  scale_y_team_size +
  scale_color_manual(values = c(colors[["team_size"]], colors[["first_place"]])) +
  coord_cartesian(xlim = c(1, 100), ylim = c(1:3)) +
  base_theme +
  ggtitle("First place teams are usually bigger")

# ---- place-from-teamsize
gg_place_from_teamsize <- ggplot(by_team_size, aes(team_size, place)) +
  geom_point(aes(size = pct), alpha = default_alpha, color = colors[["team_size"]]) +
  scale_x_continuous("team size", breaks = c(1, seq(10, 40, by = 10))) +
  scale_y_place +
  scale_size_continuous("Teams", labels = percent) +
  base_theme +
  theme(
    legend.position = "bottom"
  ) +
  ggtitle("Teammates help a little")

place_mod <- lm(place ~ entries + team_size, data = top_100)

x_preds <- expand.grid(entries = mean(top_100$entries), team_size = 1:5)
y_preds <- predict(place_mod, x_preds, se = TRUE)
preds <- cbind(x_preds, y_preds) %>%
  rename(place = fit, se = se.fit)

gg_place_from_teamsize +
  geom_smooth(aes(ymin = place - se, ymax = place + se), data = preds,
              stat = "identity", color = colors[["orange"]])

# ---- additional-teammates
max_team_size <- 4

gg_place_from_teamsize %+% filter(by_team_size, team_size <= max_team_size) +
  scale_x_continuous("team size", breaks = 1:max_team_size) +
  geom_text(aes(label = pct_label), nudge_y = 1.2) +
  guides(size = "none") +
  ggtitle("By far most submissions are from individuals")

# ---- relative-submissions-from-place
submissions_relative_first_place <- top_100 %>%
  group_by(slug) %>%
  mutate(
    submissions_relative_first_place = entries - entries[first_place == TRUE]
  ) %>%
  ungroup

ggplot(submissions_relative_first_place, aes(place, submissions_relative_first_place)) +
  geom_point(aes(color = first_place), alpha = default_alpha,
             stat = "summary", fun.y = "mean") +
  scale_x_place +
  scale_y_continuous("submissions\nrelative to first place team",
                     breaks = seq(-100, 10, by = 5)) +
  scale_color_manual(values = c(colors[["submissions"]], colors[["first_place"]])) +
  base_theme

# ---- correlation
place_breaks <- c(100, 50, 25, 1)
first_place <- by_place %>% filter(place == 1) %>% as.list
ggplot(by_place, aes(team_size, entries)) +
  geom_point(aes(color = first_place), alpha = default_alpha, size = 2) +
  scale_x_continuous("team size", breaks = 1:4) +
  scale_y_continuous("submissions", breaks = c(1, seq(5, 100, by = 5))) +
  scale_color_manual(values = c("gray", colors[["first_place"]])) +
  coord_cartesian(xlim = c(1, 3), ylim = c(1, 39)) +
  base_theme
