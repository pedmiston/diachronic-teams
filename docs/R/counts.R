source("docs/R/setup.R")

# ---- counts
condition_counts <- Teams %>%
  filter(TeamStatus == "V") %>%
  count(Exp, Strategy, SessionDuration) %>%
  arrange(desc(Exp), Strategy)

condition_counts