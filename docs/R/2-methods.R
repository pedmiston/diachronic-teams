source("docs/R/0-setup.R")
# ---- methods
data("Teams")
data("Sessions")

TeamCounts <- Teams %>%
  filter(
    SessionDuration %in% c(25, 50),
    !(Strategy == "Synchronic" & !(PlayersPerSession %in% c(2, 4)))
  ) %>%
  count(Strategy, SessionDuration, PlayersPerSession) %>%
  rename(NumTeams = n)

PlayerCounts <- Sessions %>%
  left_join(
    Teams %>%
      select(TeamID, SessionDuration, PlayersPerSession) %>%
      unique()
  ) %>%
  filter(
    SessionDuration %in% c(25, 50),
    !(Strategy == "Synchronic" & !(PlayersPerSession %in% c(2, 4)))
  ) %>%
  select(PlayerID, Strategy, SessionDuration, PlayersPerSession) %>%
  unique() %>%
  count(Strategy, SessionDuration, PlayersPerSession) %>%
  rename(NumPlayers = n)

ConditionCounts <- Teams %>%
  filter(
    SessionDuration %in% c(25, 50),
    !(Strategy == "Synchronic" & !(PlayersPerSession %in% c(2, 4)))
  ) %>%
  select(Strategy, SessionDuration, PlayersPerSession) %>%
  unique() %>%
  arrange(Strategy) %>%
  left_join(TeamCounts) %>%
  left_join(PlayerCounts)

# Reporting model results ----

report_lmer_mod <- function(lmer_mod, term, formats = NULL, reverse_sign = FALSE) {
  term_ <- term  # work around NSE in filter
  results <- broom::tidy(lmer_mod, effects = "fixed") %>%
    filter(term == term_) %>%
    as.list()
  
  if(reverse_sign) {
    results$estimate <- -results$estimate
    results$statistic <- -results$statistic
  }

  fmt <- c(b=2, se=2, t=2)
  if(!is.null(formats)) fmt[names(formats)] <- formats
  
  fstring <- sprintf("_b_ = %%.%sf (SE = %%.%sf), _t_ = %%.%sf", fmt["b"], fmt["se"], fmt["t"])
  sprintf(fstring, results$estimate, results$std.error, results$statistic)
}

report_lm_mod <- function(lm_mod, term, min_p_value = 0.001, p_value_only = FALSE) {
  term_ <- term  # work around NSE in call to filter
  results <- broom::tidy(lm_mod) %>%
    filter(term == term_) %>%
    as.list()

  lm_summary <- broom::glance(lm_mod) %>% as.list()
  results$df <- lm_summary$df.residual

  results$p_value_str <- compute_p_string(results$p.value)

  if (p_value_only == TRUE) {
    return(results$p_value_str)
  }

  sprintf("_b_ = %.2f (SE = %.2f), _t_(%.1f) = %.2f, %s",
          results$estimate, results$std.error, results$df, results$statistic, results$p_value_str)
}

report_glm_mod <- function(glm_mod, term, min_p_value = 0.001, p_value_only = FALSE) {
  term_ <- term  # work around NSE in call to filter
  results <- broom::tidy(glm_mod) %>%
    filter(term == term_) %>%
    as.list()
  
  glm_summary <- broom::glance(glm_mod) %>% as.list()
  results$df <- glm_summary$df.residual
  
  results$p_value_str <- compute_p_string(results$p.value)
  
  sprintf("_b_ = %.2f logodds (SE = %.2f), _z_ = %.2f, %s",
          results$estimate, results$std.error, results$statistic, results$p_value_str)
}

report_beta <- function(mod, param, digits = 1, transform = NULL) {
  param_ <- param # prevent masking in NSE
  estimate <- mod %>%
    summary %>%
    .$coefficients %>%
    as.data.frame() %>%
    rownames_to_column("param") %>%
    filter(param == param_) %>%
    .$Estimate
  if(!is.null(transform)) estimate <- transform(estimate)
  round(estimate, digits = digits)
}

report_modcomp <- function(modcomp) {
  modcomp <- as.list(modcomp[2, ])
  p_string <- compute_p_string(modcomp$`Pr(>Chisq)`)
  print(sprintf("$\\chi^2$(%i) = %.4f, %s", modcomp$`Chi Df`, modcomp$Chisq, p_string))
}

report_page_test <- function(page_trend_test_results) {
  page_trend_test_results$p_val_str <- compute_p_string(page_trend_test_results$px2)
  print(sprintf("Page's _L_ = %.0f, $\\chi^2$ = %.0f, %s",
              page_trend_test_results$L,
              page_trend_test_results$x2L,
              page_trend_test_results$p_val_str))
}

compute_p_string <- function(p_value) {
  min_p_value <- 0.001
  if (p_value < min_p_value) {
    p_value_str <- "_p_ < 0.001"
  } else {
    p_value_str <- paste("_p_ = ", round(p_value, 3))
  }
  p_value_str
}

# Jitter Generation by TeamID for plotting
jitter_team_generation <- . %>%
  group_by(TeamID) %>%
  mutate(GenerationJittered = Generation + rnorm(1, mean = 0, sd = 0.05)) %>%
  ungroup()

# Recode Generation poly
recode_generation_quad <- . %>%
  mutate(
    GenerationSqr = Generation^2,
    Generation0Sqr = Generation0^2
  )

recode_generation_base0 <- . %>%
  mutate(Generation0 = Generation - 1)