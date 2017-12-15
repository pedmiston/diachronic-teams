source("R/totems/0-setup.R")
# ---- methods

# Condition counts ----
data("Teams")
data("Sessions")

TeamCounts <- Teams %>%
  filter(
    TeamStatus == "V",
    !(Strategy == "Isolated" & SessionsPerPlayer == 2),
    !(Strategy == "Diachronic" & NumPlayers == 2)
  ) %>%
  rename(TeamSize = NumPlayers) %>%
  count(Strategy, SessionDuration, TeamSize) %>%
  rename(NumTeams = n)

PlayerCounts <- Players %>%
  left_join(
    Teams %>%
      filter(
        TeamStatus == "V",
        !(Strategy == "Isolated" & SessionsPerPlayer == 2),
        !(Strategy == "Diachronic" & NumPlayers == 2)
      ) %>%
      select(TeamID, SessionDuration, TeamSize = NumPlayers, SessionsPerPlayer, PlayersPerSession) %>%
      unique()
  ) %>%
  filter(
    TeamStatus == "V",
    !(Strategy == "Isolated" & SessionsPerPlayer == 2),
    !(Strategy == "Diachronic" & NumPlayers == 2)
  ) %>%
  select(PlayerID, Strategy, SessionDuration, TeamSize, SessionsPerPlayer, PlayersPerSession) %>%
  unique() %>%
  count(Strategy, SessionDuration, TeamSize) %>%
  rename(NumPlayers = n)

ConditionCounts <- Teams %>%
  filter(
    TeamStatus == "V",
    !(Strategy == "Isolated" & SessionsPerPlayer == 2),
    !(Strategy == "Diachronic" & NumPlayers == 2)
  ) %>%
  select(Strategy, SessionDuration, TeamSize = NumPlayers, SessionsPerPlayer, PlayersPerSession) %>%
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