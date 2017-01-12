library(devtools)
library(readr)

bots1 <- read_csv("data-raw/bots/experiment-1.csv")
use_data(bots1, overwrite = TRUE)
