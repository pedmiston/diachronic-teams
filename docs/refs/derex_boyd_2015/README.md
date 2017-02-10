# Represent inventions as a graph

## Create nodes for items in a graph database

Requires Neo4j to be installed and running. Also requires your Neo4j password to be set as an environment variable named `NEO4J_PASSWORD`.

```bash
neo4j start
export NEO4J_PASSWORD=my-secret-neo4j-password
python inventions.py
```

## Draw the invention tree

Once the items are in the database, draw the invention tree with:

```bash
python tree.py  # creates items.gv.pdf and opens it for viewing
```
