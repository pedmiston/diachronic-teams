source("docs/R/setup.R")

# ---- sample-landscape
library(RNeo4j)

graph <- startGraph("http://localhost:7474/db/data/",
                    username = "neo4j",
                    password = Sys.getenv("NEO4J_PASSWORD"))
browse(graph)
