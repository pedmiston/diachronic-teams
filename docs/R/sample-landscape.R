source("docs/R/setup.R")

# ---- sample-landscape
# library(RNeo4j)
# graph <- startGraph("http://localhost:7474/db/data",
#                     username = "neo4j",
#                     password = Sys.getenv("NEO4J_PASSWORD"))
# browse(graph)

data("BotsExplore")

sample_trajectories <- BotsExplore %>%
  filter(inventory_size <= 15) %>%
  select(sim_id, inventory, inventory_size, trajectory) %>%
  group_by(inventory_size) %>%
  summarize(n_unique_trajectories = length(unique(trajectory)))

unique_trajectories_plot <- ggplot(sample_trajectories) +
  aes(inventory_size, n_unique_trajectories, group = 1) +
  geom_line()
