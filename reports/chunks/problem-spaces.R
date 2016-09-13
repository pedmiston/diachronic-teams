# ---- problem-spaces
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)

problem_space_levels <- c("grueling", "rugged", "insight")

x_range <- seq(-5, 5, length.out = 100)

curves <- list(
  grueling = function(x) -x^2,
  rugged = function(x) {
    # brownian noise
    sig2 <- 0.01
    noise <- rnorm(length(x) - 1, sd = sqrt(sig2))
    c(0, cumsum(noise))
  },
  insight = function(x) ifelse(x < 3 | x > 4, 0, -abs(x - 3.6) * 10 + 10)
)
to_z <- function(x) (x - mean(x))/sd(x)

set.seed(683)  # reproducible randomness
mountains <- lapply(curves, function(curve_fn) curve_fn(x_range) %>% to_z) %>%
  as.data.frame %>%
  mutate(x = x_range) %>%
  gather(problem_space, y, -x) %>%
  mutate(
    problem_space = factor(problem_space, levels = problem_space_levels,
                           labels = str_to_title(problem_space_levels)),
    # lower insight problem. NB: labels are title case
    y = ifelse(problem_space == "Insight", y - 1.8, y)
  )

ggplot(mountains, aes(x, y)) +
  geom_line(aes(group = problem_space)) +
  facet_wrap("problem_space") +
  scale_x_continuous("Possible solutions", labels = NULL) +
  scale_y_continuous("Solution success", labels = NULL) +
  theme_minimal() +
  theme(
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ggtitle("Problem spaces")