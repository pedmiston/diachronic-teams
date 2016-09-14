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
      pct = n/sum(n)      
    )
}

place_by_team_size <- top_100 %>%
  group_by(team_size) %>%
  summarize_by_place

place_by_submissions <- top_100 %>%
  group_by(entries) %>%
  summarize_by_place

# theme
base_theme <- theme_minimal() +
  theme(
    legend.position = "none",
    axis.ticks = element_blank(),
    panel.grid.minor.x = element_blank()
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

# ---- submissions-by-place
by_place <- top_100 %>%
  group_by(first_place, place) %>%
  summarize(
    entries = mean(entries),
    team_size = mean(team_size)
  )

gg_entries_by_place <- ggplot(by_place, aes(place, entries)) +
  geom_point(aes(color = first_place), alpha = default_alpha) +
  scale_x_place +
  scale_y_submissions +
  scale_color_manual(values = c(colors[["submissions"]], colors[["first_place"]])) +
  coord_cartesian(xlim = c(1, 100), ylim = c(1, 39)) +
  base_theme +
  ggtitle("Number of submissions")
gg_entries_by_place

# ---- relative-submissions-by-place
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

# ---- by-place
gg_team_size_by_place <- ggplot(by_place, aes(place, team_size)) +
  geom_point(aes(color = first_place), alpha = default_alpha) +
  scale_x_place +
  scale_y_team_size +
  scale_color_manual(values = c(colors[["team_size"]], colors[["first_place"]])) +
  coord_cartesian(xlim = c(1, 100), ylim = c(1:3)) +
  base_theme +
  ggtitle("Size of team")

grid.arrange(gg_entries_by_place, gg_team_size_by_place, ncol = 2)

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

# ---- predicting-place-by-submissions
gg_place_by_submissions <- ggplot(place_by_submissions, aes(entries, place)) +
  geom_point(aes(size = pct), alpha = default_alpha, color = colors[["submissions"]]) +
  scale_x_continuous("submissions", breaks = c(1, seq(100, 600, by = 100))) +
  scale_y_place +
  scale_size_continuous("Teams", labels = percent) +
  coord_cartesian(xlim = c(1, 600), ylim = c(1, 100)) +
  base_theme +
  theme(
    legend.position = "bottom"
  )

gg_place_by_submissions

# ---- predicting-place-mod
place_mod <- lm(place ~ entries + team_size, data = top_100)

# ---- predicting-place-by-submissions-with-error
x_preds <- expand.grid(entries = 1:200, team_size = 1)
y_preds <- predict(place_mod, x_preds, se = TRUE)
preds <- cbind(x_preds, y_preds) %>%
  rename(place = fit, se = se.fit)

gg_place_by_submissions +
  geom_smooth(aes(ymin = place - se, ymax = place + se), data = preds,
              stat = "identity", color = colors[["orange"]])

# ---- predicting-place
gg_place_by_team_size <- ggplot(place_by_team_size, aes(team_size, place)) +
  geom_point(aes(size = pct), alpha = default_alpha, color = colors[["team_size"]]) +
  scale_x_continuous("team size", breaks = c(1, seq(10, 40, by = 10))) +
  scale_y_place +
  scale_size_continuous("Teams", labels = percent) +
  base_theme +
  theme(
    legend.position = "bottom"
  )

grid.arrange(gg_place_by_submissions, gg_place_by_team_size, ncol = 2)
