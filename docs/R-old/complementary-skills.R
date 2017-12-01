source("docs/R/setup.R")
# ---- complementary-skills
data("Guesses")

# How well do people correlate for guess difficulty?
#
# Guess Difficulty: For each inventory, how many guesses
# does it take to get each of the items that could be
# reached in one move from that inventory?
#
# Data that needs to be added to "Guesses":
# - Player and Team inventory when Guess was made
# - A str representing all adjacent items not already in the inventory, e.g., "9-11-13"
# 
# For each Invention, count the number of (incorrect) guesses that
# were made when the Invention was adjacent to the current inventory.
