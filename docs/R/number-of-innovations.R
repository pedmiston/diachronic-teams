source("docs/R/setup.R")

# ---- num-innovations-50

# Performance over time
data("Sampled")

highlight_inheritance <- function(frame) {
  highlight_inheritance_map <- expand.grid(
      Strategy = c("Diachronic", "Isolated", "Synchronic"),
      Generation = 1:4,
      stringsAsFactors = FALSE
    ) %>%
    dplyr::filter(!(Strategy == "Synchronic" & Generation > 1)) %>%
    dplyr::mutate(Inheritance = ifelse(Strategy == "Diachronic" & Generation > 1,
                  "diachronic_inheritance", "no_inheritance")) %>%
    dplyr::arrange(Strategy, Generation)
  if(missing(frame)) return(highlight_inheritance_map)
  left_join(frame, highlight_inheritance_map)
}

Sampled50 <- Sampled %>%
  recode_strategy() %>%
  highlight_inheritance() %>%
  mutate(NumInnovations = InventorySize - 6) %>%
  filter(
    TeamStatus == "V",
    Exp == "50LaborMinutes"
  )

scale_x_labor_time_50 <- scale_x_continuous(
  "Labor time (min)",
  breaks = seq(0, 3000, by = 60 * 5),
  labels = seq(0, 50, by = 5))

scale_y_num_innovations <- scale_y_continuous(
  "Number of innovations",
  breaks = seq(0, 30, by = 2))

num_innovations_over_time_50 <- ggplot(Sampled50) +
  aes(TeamTime, NumInnovations) +
  geom_line(aes(color = StrategyLabel,
                group = interaction(StrategyLabel, SessionDuration, Generation),
                linetype = factor(SessionDuration),
                size = Inheritance),
            stat = "summary", fun.y = "mean") +
  totems_theme$scale_color_strategy +
  scale_size_manual(values = c(1.8, 1.0)) +
  scale_x_labor_time_50 +
  scale_y_num_innovations +
  guides(linetype = "none", size = "none") +
  totems_theme$base_theme +
  theme(legend.position = c(0.15, 0.8))

label <- data_frame(
  Text = "Diachronic inheritance",
  Strategy = "Diachronic",
  TeamTime = 32 * 60,
  NumInnovations = 5
)

num_innovations_over_time_50 <- num_innovations_over_time_50 +
  geom_text(aes(label = Text), data = label,
            color = totems_theme$diachronic_color,
            fontface = "bold",
            hjust = 0)

# Final performance
data("TeamPerformance")

TeamPerformance50 <- TeamPerformance %>%
  filter(
    TeamStatus == "V",
    Exp == "50LaborMinutes"
  ) %>%
  recode_strategy()

set.seed(432)
final_num_innovations_50 <- ggplot(TeamPerformance50) +
  aes(Strategy, NumInnovations, group = SessionDuration) +
  geom_bar(aes(fill = Strategy, group = SessionDuration),
           position = position_dodge(),
           stat = "summary", fun.y = "mean", alpha = 0.4) +
  geom_point(aes(color = Strategy, shape = factor(SessionDuration)),
             position = position_jitterdodge(jitter.height = 0.2,
                                             jitter.width = 0.4,
                                             dodge.width = 1.01)) +
  scale_y_num_innovations +
  scale_shape_manual(values = c(16, 1)) +
  theme(legend.position = "none")


data("PlayerPerformance")

NumInnovationsByGeneration50 <- PlayerPerformance %>%
  recode_strategy() %>%
  filter(
    TeamStatus == "V",
    Exp == "50LaborMinutes",
    Strategy != "Synchronic",
    SessionDuration == 25
  )

num_innovations_by_generation_50_mod <- lmer(
  NumInnovations ~ Diachronic_v_Isolated * Generation + (1|TeamID),
  data = NumInnovationsByGeneration50
)

num_innovations_by_generation_50_preds <- expand.grid(
  Generation = 1:2,
  Strategy = c("Diachronic", "Isolated"),
  stringsAsFactors = FALSE
) %>%
  recode_strategy() %>%
  cbind(., predictSE(num_innovations_by_generation_50_mod, newdata = ., se = TRUE)) %>%
  rename(NumInnovations = fit, SE = se.fit)

num_innovations_by_generation_50_plot <- ggplot(NumInnovationsByGeneration50) +
  aes(Generation, NumInnovations) +
  geom_line(aes(group = TeamID), alpha = 0.4) +
  geom_line(stat = "identity", size = 2,
            data = num_innovations_by_generation_50_preds) +
  geom_errorbar(aes(ymin = NumInnovations-SE, ymax = NumInnovations+SE),
                data = num_innovations_by_generation_50_preds,
                width = 0.2, size = 2) +
  scale_x_continuous("Generation", breaks = 1:2) +
  facet_wrap("Strategy")