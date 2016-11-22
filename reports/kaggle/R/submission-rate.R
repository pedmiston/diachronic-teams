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
  labs(title = "Submission intervals are consistent across places")

# ---- prop-time-per-place

# ---- time-by-submission-density
new_density_plot <- function(leaderboards) {
  ggplot(leaderboards, aes(TotalSubmissions, TotalTimeSec)) +
    geom_point(color = colors[["blue"]], alpha = 0.2) +
    geom_hline(yintercept = median(leaderboards$TotalTimeSec),
               color = "gray") +
    geom_vline(xintercept = median(leaderboards$TotalSubmissions),
               color = "gray") +
    scale_x_total_submissions +
    make_time_scale("submission interval (days)", seq(0, 400, by = 100)) +
    base_theme
}

new_density_plot(top_100) +
  labs(title = "Team type density for top 100 place finishes")

top_10 <- leaderboards %>%
  filter(Place <= 10) %>%
  label_place_groups()
new_density_plot(top_10) +
  labs(title = "Team type density for top 10 place finishes")

# ---- time-by-submission-top-100
ggplot(top_100_places, aes(TotalSubmissions, TotalTime)) +
  #geom_point(aes(alpha = Place, color = FirstPlaceTeam)) +
  geom_text(aes(label = Place, alpha = Place), size = 2, check_overlap = TRUE) +
  scale_x_continuous("total submissions", breaks = c(1, seq(5, 40, by = 5))) +
  scale_color_manual(values = c("black", colors[["first_place"]])) +
  scale_alpha_continuous(range = c(1.0, 0.2)) +
  coord_cartesian(xlim = c(1, 35), ylim = c(0, 25 * 24 * 3600)) +
  make_time_scale("submission interval (days)", seq(0, 100, by = 5)) +
  base_theme

# ---- prop-time-by-rel-submissions-top-100
