source("docs/R/setup.R")

# ---- counts
TeamSizes <- Teams %>%
  select(Exp, Strategy, SessionDuration, PlayersPerTeam = NumPlayers, SessionsPerPlayer) %>%
  unique()

condition_counts <- Teams %>%
  filter(TeamStatus == "V") %>%
  count(Exp, Strategy, SessionDuration) %>%
  arrange(desc(Exp), Strategy, desc(SessionDuration)) %>%
  rename(NumTeams = n) %>%
  left_join(TeamSizes) %>%
  recode_experiment() %>%
  mutate(NumPlayers = PlayersPerTeam * NumTeams) %>%
  select(Exp, Strategy, PlayersPerTeam, SessionDuration, SessionsPerPlayer, NumTeams, NumPlayers)
