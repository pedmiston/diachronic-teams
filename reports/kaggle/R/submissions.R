source("reports/kaggle/R/setup.R")
source("reports/kaggle/R/theme.R")

# ---- submissions-from-place
ggplot(by_place, aes(Place, TotalSubmissions)) +
  geom_point(aes(color = FirstPlaceTeam), alpha = default_alpha) +
  scale_x_place +
  scale_y_submissions +
  scale_color_manual(values = c(colors[["submissions"]], colors[["first_place"]])) +
  coord_cartesian(xlim = top_100_places_xlim, ylim = top_100_submissions_ylim) +
  base_theme +
  labs(title = "Top place teams make more submissions")

# ---- relative-submissions-from-place
submissions_relative_first_place <- top_100 %>%
  group_by(CompetitionId) %>%
  mutate(
    SubmissionsToFirstPlace = TotalSubmissions - TotalSubmissions[FirstPlaceTeam == TRUE]
  ) %>%
  ungroup()

ggplot(submissions_relative_first_place, aes(Place, SubmissionsToFirstPlace)) +
  geom_point(aes(color = FirstPlaceTeam), alpha = default_alpha,
             stat = "summary", fun.y = "mean") +
  scale_x_place +
  scale_y_continuous("submissions\nrelative to first place team",
                     breaks = seq(-100, 10, by = 5)) +
  scale_color_manual(values = c(colors[["submissions"]], colors[["first_place"]])) +
  coord_cartesian(xlim = top_100_places_xlim, ylim = top_100_submissions_ylim - 40) +
  base_theme +
  labs(title = "Top place teams make more submissions")

# ---- place-from-submissions
gg_place_from_submissions <- ggplot(by_submissions, aes(TotalSubmissionsBin, Place)) +
  geom_point(aes(size = PercentTeams), alpha = default_alpha,
             color = colors[["submissions"]]) +
  scale_x_total_submissions +
  scale_y_place +
  scale_size_continuous("proportion of teams", labels = percent, breaks = rev(c(0.01, 0.05, 0.15, 0.6))) +
  coord_cartesian(xlim = c(1, 600), ylim = c(1, 100)) +
  base_theme +
  theme(legend.position = "bottom") +
  ggtitle("Making more submissions improves place")

place_mod <- lm(Place ~ TotalSubmissions + TeamSize, data = top_100)

x_preds <- expand.grid(TotalSubmissions = 1:200, TeamSize = 1)
y_preds <- predict(place_mod, x_preds, se = TRUE)
preds <- cbind(x_preds, y_preds) %>%
  rename(Place = fit, SE = se.fit) %>%
  mutate(TotalSubmissionsBin = TotalSubmissions)  # for consistency with summary

gg_place_from_submissions +
  geom_smooth(aes(ymin =  Place - SE, ymax =  Place + SE), data = preds,
              stat = "identity", color = colors[["orange"]])

# ---- predicted-place-from-submission

sample_teams <- function(n_teams = 1, min_submissions = 50, 
                         min_final_place = 100, seed = NA) {
  
  if (!is.na(seed)) set.seed(seed)
  
  team_ids <- leaderboards %>%
    filter(
      TotalSubmissions >= min_submissions,
      Place <= min_final_place
    ) %>%
    sample_n(n_teams) %>%
    .$TeamId
  
  submissions %>% filter(TeamId %in% team_ids)
}

n_teams <- 200
submissions_sample <- sample_teams(n_teams = n_teams, seed = 821)

team_submissions_plot <- ggplot(submissions_sample, aes(SubmissionNum, PredictedPlace)) +
  geom_smooth(aes(group = TeamId), method = "lm", se = FALSE,
              size = 0.4, alpha = 0.4, color = colors[["submissions"]]) +
  scale_x_submission_number +
  scale_y_reverse("place", breaks = c(1, 500, seq(1000, 5000, by = 1000))) +
  base_theme +
  theme(legend.position = "none") +
  labs(title = paste("Changes in performance for", n_teams, "teams"))

team_submissions_mod <- lmer(PredictedPlace ~ SubmissionNum + 
                               (SubmissionNum|TeamId) + (1|CompetitionId/TeamId),
                             data = submissions_sample)

team_submissions_preds <- data_frame(SubmissionNum = 1:100) %>%
  cbind(., predictSE(team_submissions_mod, newdata = .)) %>%
  rename(PredictedPlace = fit, SE = se.fit)

team_submissions_plot +
  geom_smooth(aes(ymin = PredictedPlace - SE, ymax = PredictedPlace + SE),
              data = team_submissions_preds, color = colors[["orange"]],
              size = 1.5)
