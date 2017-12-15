source("R/totems/0-setup.R")
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
    SessionStatus == "valid",
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

report_lmer_mod <- function(lmer_mod, term) {
  term_ <- term  # work around NSE in filter
  results <- broom::tidy(lmer_mod, effects = "fixed") %>%
    filter(term == term_) %>%
    as.list()

  sprintf("_b_ = %.2f (SE = %.2f), _t_ = %.2f",
          results$estimate, results$std.error, results$statistic)
}

report_lm_mod <- function(lm_mod, term, min_p_value = 0.001, p_value_only = FALSE) {
  term_ <- term  # work around NSE in call to filter
  results <- broom::tidy(lm_mod) %>%
    filter(term == term_) %>%
    as.list()

  lm_summary <- broom::glance(lm_mod) %>% as.list()
  results$df <- lm_summary$df.residual

  if (results$p.value < min_p_value) {
    results$p_value_str <- "_p_ < 0.001"
  } else {
    results$p_value_str <- paste("_p_ = ", round(results$p.value, 3))
  }

  if (p_value_only == TRUE) {
    return(results$p_value_str)
  }

  sprintf("_b_ = %.2f (SE = %.2f), _t_(%.1f) = %.2f, %s",
          results$estimate, results$std.error, results$df, results$statistic, results$p_value_str)
}

report_beta <- function(mod, param, digits = 1) {
  param_ <- param # prevent masking in NSE
  mod %>%
    summary %>%
    .$coefficients %>%
    as.data.frame() %>%
    rownames_to_column("param") %>%
    filter(param == param_) %>%
    .$Estimate %>%
    round(digits = digits)
}
