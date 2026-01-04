# descriptives.R

default_palettes <- function() {
  list(
    pal_target = c(self = "#E15759", climate = "#59A14F", prosocial = "#4E79A7"),
    pal_group  = c(control = "#4E79A7", positive_norm = "#59A14F", negative_norm = "#E15759"),
    group_labels = c(
      control = "Control Group",
      positive_norm = "Positive Norm",
      negative_norm = "Negative Norm"
    )
  )
}

summarise_choice_rates <- function(df_long) {
  overall <- df_long |>
    dplyr::summarise(
      n_trials = dplyr::n(),
      n_valid = sum(!is.na(choice)),
      n_missing = sum(is.na(choice)),
      mean_choice = mean(choice, na.rm = TRUE),
      sd_choice = sd(choice, na.rm = TRUE)
    ) |>
    dplyr::mutate(level = "overall", group = NA, block = NA, target = NA)
  
  by_target <- df_long |>
    dplyr::group_by(target) |>
    dplyr::summarise(
      n_trials = dplyr::n(),
      n_valid = sum(!is.na(choice)),
      n_missing = sum(is.na(choice)),
      mean_choice = mean(choice, na.rm = TRUE),
      sd_choice = sd(choice, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(level = "by_target", group = NA, block = NA)
  
  by_group_block <- df_long |>
    dplyr::group_by(group, block) |>
    dplyr::summarise(
      n_trials = dplyr::n(),
      n_valid = sum(!is.na(choice)),
      n_missing = sum(is.na(choice)),
      mean_choice = mean(choice, na.rm = TRUE),
      sd_choice = sd(choice, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(level = "by_group_block", target = NA)
  
  by_all <- df_long |>
    dplyr::group_by(target, group, block) |>
    dplyr::summarise(
      n_trials = dplyr::n(),
      n_valid = sum(!is.na(choice)),
      mean_choice = mean(choice, na.rm = TRUE),
      sd_choice = sd(choice, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(level = "by_target_group_block", n_missing = NA)
  
  dplyr::bind_rows(overall, by_target, by_group_block, by_all)
}

plot_reward_effort <- function(df_long) {
  plot_a_data <- df_long |>
    dplyr::group_by(reward, effort) |>
    dplyr::summarise(p = mean(choice == 1, na.rm = TRUE), .groups = "drop")
  
  ggplot(plot_a_data, aes(x = reward, y = p, color = effort, group = effort)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = "Reward", y = "Proportion of High-Effort Choices", color = "Effort") +
    theme_bw()
}

plot_target_block_group <- function(df_long, zoom = FALSE) {
  pals <- default_palettes()
  plot_b_data <- df_long |>
    dplyr::group_by(group, block, target) |>
    dplyr::summarise(
      p_choice = mean(choice == 1, na.rm = TRUE),
      n = sum(!is.na(choice)),
      .groups = "drop"
    )
  
  p <- ggplot(plot_b_data, aes(x = block, y = p_choice, color = target, group = target)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    facet_wrap(~ group, labeller = labeller(group = pals$group_labels)) +
    labs(x = "Block", y = "Proportion of High-Effort Choices", color = "Target") +
    scale_color_manual(values = pals$pal_target) +
    theme_bw() +
    theme(legend.position = "top", legend.title = element_blank())
  
  if (zoom) {
    p <- p + scale_y_continuous(limits = c(0.85, 1), breaks = seq(0.85, 1, 0.025))
  } else {
    p <- p + scale_y_continuous(limits = c(0, 1))
  }
  
  p
}

ceiling_summary_table <- function(df_long) {
  df_long |>
    dplyr::group_by(ppn) |>
    dplyr::summarise(
      n_trials = dplyr::n(),
      n_valid = sum(!is.na(choice)),
      prop = dplyr::if_else(n_valid > 0, mean(choice, na.rm = TRUE), NA_real_),
      sd_within = dplyr::if_else(n_valid > 1, sd(choice, na.rm = TRUE), NA_real_),
      .groups = "drop"
    ) |>
    dplyr::filter(!is.na(prop))
}