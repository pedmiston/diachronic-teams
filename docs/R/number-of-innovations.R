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
data("PlayerPerformance")

recode_session_type_50 <- function(frame) {
  session_type_levels <- c(
    "DG1", "DG2",
    "I50", "IS1", "IS2",
    "S2"
  )
  
  session_type_labels <- c(
    "Diachronic G1",
    "Diachronic G2",
    "Isolated 50min",
    "Isolated S1",
    "Isolated S2",
    "Synchronic"
  )

  session_type_map <- data_frame(
    Strategy = c(rep("Diachronic", 2), rep("Isolated", 3), "Synchronic"),
    Generation = c(1:2, 1, 1:2, 1),
    SessionDuration = c(rep(25, 2), 50, rep(25, 3)),
    NumPlayers = c(rep(2, 2), rep(1, 3), 2),
    SessionType = session_type_levels,
    SessionTypeOrdered = factor(session_type_levels, levels = session_type_levels, labels = session_type_labels),
    SessionTypeTreat = factor(session_type_levels, levels = session_type_levels)
  )
  
  # Set treatment contrasts for SessionType with D-G2 as base comparison group.
  session_type_treat_contrasts <- contr.treatment(factor(session_type_levels, levels = session_type_levels),
                                                  base = 2)
  contrasts(session_type_map$SessionTypeTreat) <- session_type_treat_contrasts
  
  session_type_treat_contrasts <- as.data.frame(session_type_treat_contrasts)
  contrast_names <- colnames(session_type_treat_contrasts) %>%
    purrr::map(function(x) paste0("DG2_v_", x)) %>%
    unlist()
  colnames(session_type_treat_contrasts) <- contrast_names
  session_type_treat_contrasts <- session_type_treat_contrasts %>%
    rownames_to_column("SessionType")
  session_type_map <- left_join(session_type_map, session_type_treat_contrasts)

  if(missing(frame)) return(session_type_map)
  left_join(frame, session_type_map)
}

PlayerPerformance50 <- PlayerPerformance %>%
  recode_strategy() %>%
  recode_session_type_50() %>%
  highlight_inheritance() %>%
  filter(
    TeamStatus == "V",
    Exp == "50LaborMinutes"
  )

final_num_innovations_50_mod <- lm(
  NumInnovations ~ DG2_v_DG1 + DG2_v_S2 + DG2_v_I50 + DG2_v_IS1 + DG2_v_IS2,
  data = PlayerPerformance50)

final_num_innovations_50_preds <- recode_session_type_50() %>%
  cbind(., predict(final_num_innovations_50_mod, newdata = ., se = TRUE)) %>%
  rename(NumInnovations = fit, SE = se.fit) %>%
  recode_strategy() %>%
  highlight_inheritance()

set.seed(432)
final_num_innovations_50_plot <- ggplot(PlayerPerformance50) +
  aes(SessionTypeOrdered, NumInnovations) +
  geom_bar(aes(fill = StrategyLabel, alpha = Inheritance),
           stat = "identity", data = final_num_innovations_50_preds) +
  geom_point(aes(color = StrategyLabel),
             position = position_jitter(width = 0.3)) +
  geom_linerange(aes(ymin = NumInnovations-SE, ymax = NumInnovations+SE),
                 data = final_num_innovations_50_preds) +
  totems_theme$scale_color_strategy +
  totems_theme$scale_fill_strategy +
  scale_alpha_manual(values = c(0.7, 0.4)) +
  xlab("") +
  scale_y_num_innovations +
  scale_shape_manual(values = c(16, 1)) +
  totems_theme$base_theme +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  ) +
  ggtitle("Final number of innovations discovered by each strategy")


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