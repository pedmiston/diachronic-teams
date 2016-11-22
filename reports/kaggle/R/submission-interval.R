source("reports/kaggle/R/setup.R")
source("reports/kaggle/R/theme.R")

# ---- time-per-place
ggplot(top_100_places, aes(Place, TotalTime)) +
  geom_point(aes(color = FirstPlaceTeam), alpha = default_alpha) +
  scale_x_place +
  make_time_scale("submission interval (days)", breaks_days = seq(0, 30, by = 5)) +
  scale_color_manual(values = c(colors[["green"]], colors[["first_place"]])) +
  coord_cartesian(xlim = top_100_places_xlim, ylim = c(0, 30 * 24 * 3600)) +
  base_theme +
  labs(title = "Consistent submission intervals across places")

# ---- prop-time-per-place
ggplot(top_100_places, aes(Place, PropCompetitionTime)) +
  geom_point(aes(color = FirstPlaceTeam), alpha = default_alpha) +
  scale_x_place +
  scale_y_continuous("proportion time used", labels = percent) +
  scale_color_manual(values = c(colors[["green"]], colors[["first_place"]])) +
  coord_cartesian(xlim = top_100_places_xlim, ylim = c(0, 1)) +
  base_theme
