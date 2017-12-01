source("docs/R/setup.R")

# ---- sample-landscape

scale_x_inventory_size <- scale_x_continuous("Inventory size",
                                             breaks = 6:15)

# Trajectories plot
data("BotsExplore")
sample_trajectories <- BotsExplore %>%
  filter(inventory_size <= 15) %>%
  select(sim_id, inventory, inventory_size, trajectory, n_adjacent) %>%
  group_by(inventory_size) %>%
  summarize(n_unique_trajectories = length(unique(trajectory)))

unique_trajectories_plot <- ggplot(sample_trajectories) +
  aes(inventory_size, n_unique_trajectories, group = 1) +
  geom_line(size = 1.2, color = "gray") +
  scale_x_inventory_size +
  scale_y_continuous("Unique trajectories", breaks = c(1, seq(10, 80, by = 10))) +
  t_["base_theme"] +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

# Combinatorial explosion plot
calculate_num_guesses <- function(step_info) {
  n <- step_info$inventory_size
  r <- step_info$guess_size
  factorial(n)/(factorial(r) * factorial(n-r))
}

possible_guesses <- expand.grid(inventory_size = 6:15,
                                guess_size = 1:4) %>%
  by_row(calculate_num_guesses, .to = "n_combinations",
         .collate = "rows") %>%
  group_by(inventory_size) %>%
  summarize(n_combinations = sum(n_combinations)) %>%
  mutate(n_new_combinations = n_combinations - lag(n_combinations, 1, default = 0)) %>%
  gather(combination_type, combination_value, -inventory_size)

correct_combinations <- BotsExplore %>%
  filter(inventory_size <= 15) %>%
  select(inventory_size, n_adjacent) %>%
  unique() %>%
  group_by(inventory_size) %>%
  summarize(combination_value = median(n_adjacent)) %>%
  mutate(combination_type = "n_correct_combinations")
possible_guesses %<>% bind_rows(correct_combinations)

combination_type_labels <- data_frame(
  combination_type = c("n_combinations", "n_new_combinations", "n_correct_combinations"),
  label = c("Total", "New", "Correct"),
  inventory_size = 13,
  combination_value = c(750, 450, 100)
)

possible_guesses_plot <- ggplot(possible_guesses) +
  aes(inventory_size, combination_value, color = combination_type) +
  geom_line(size = 1.2) +
  geom_text(aes(label = label), data = combination_type_labels) +
  # Uncomment to add text labels showing the number of correct combinations
  # geom_label(aes(label = combination_value),
  #            data = filter(possible_guesses, combination_type == "n_correct_combinations")) +
  scale_x_inventory_size +
  scale_y_continuous("Combinations") +
  scale_color_manual(
    # alphabetical: n_combinations, n_correct_combinations, n_new_combinations
    values = c(t_$synchronic_color, t_$isolated_color, t_$diachronic_color)
  ) +
  t_["base_theme"] +
  theme(
    legend.position = "none",
    panel.grid.minor.x = element_blank()
  )

grid.arrange(
  read_graphviz_with_images("landscape-sample", "totems"),
  arrangeGrob(possible_guesses_plot, unique_trajectories_plot, ncol = 1),
  nrow = 1,
  widths = c(0.6, 0.4)
)
