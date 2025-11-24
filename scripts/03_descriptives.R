# 03_descriptives

# Descriptive statistics, ceiling effects, and main descriptive plots


# Load data


df_wide <- readRDS(here::here("data","processed","df_wide.rds"))
df_long <- readRDS(here::here("data","processed","df_long.rds"))



# Helper functions



# Color palettes consistent across plots

pal_target <- c(
  "self"      = "#E15759",
  "climate"   = "#59A14F",
  "prosocial" = "#4E79A7"
)

pal_group <- c(
  "control"       = "#4E79A7",
  "positive_norm" = "#59A14F",
  "negative_norm" = "#E15759"
)



pd <- position_dodge(width = 0.7)


group_labels <- c(
  "control"       = "Control Group",
  "positive_norm" = "Positive Norm",
  "negative_norm" = "Negative Norm"
)


dir.create("figures/descriptives", recursive = TRUE, showWarnings = FALSE)


# Descriptive Statistics

## 1. Sample Characteristics: included in the rmd file if needed


## 2. Primary Outcome: Choice Behaviour


# Overall choice rates
df_long |> 
  summarise(
    n_trials = n(),
    n_valid = sum(!is.na(choice)),
    n_missing = sum(is.na(choice)),
    mean_choice = mean(choice, na.rm = TRUE),
    sd_choice = sd(choice, na.rm = TRUE)
  )

# By target type
df_long |> 
  group_by(target) |> 
  summarise(
    n_trials = n(),
    n_valid = sum(!is.na(choice)),
    n_missing = sum(is.na(choice)),
    mean_choice = mean(choice, na.rm = TRUE),
    sd_choice = sd(choice, na.rm = TRUE)
  )

# By group and block (most important!)
df_long |> 
  group_by(group, block) |> 
  summarise(
    n_trials = n(),
    n_valid = sum(!is.na(choice)),
    n_missing = sum(is.na(choice)),
    mean_choice = mean(choice, na.rm = TRUE),
    sd_choice = sd(choice, na.rm = TRUE),
    .groups = "drop"
  )

# By all three factors 
df_long |> 
  group_by(target, group, block) |> 
  summarise(
    n_trials = n(),
    n_valid = sum(!is.na(choice)),
    M = mean(choice, na.rm = TRUE),
    SD = sd(choice, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  arrange(target, group, block)




## 3. Task Controls: Reward and Effort


# Choice rates by reward level
df_long |> 
  group_by(reward) |> 
  summarise(
    n_trials = n(),
    n_valid = sum(!is.na(choice)),
    mean_choice = mean(choice, na.rm = TRUE),
    sd_choice = sd(choice, na.rm = TRUE)
  )

# Choice rates by effort level
df_long |> 
  group_by(effort) |> 
  summarise(
    n_trials = n(),
    n_valid = sum(!is.na(choice)),
    mean_choice = mean(choice, na.rm = TRUE),
    sd_choice = sd(choice, na.rm = TRUE)
  )

# Reward × Effort interaction pattern
df_long |> 
  group_by(reward, effort) |> 
  summarise(
    n_trials = n(),
    n_valid = sum(!is.na(choice)),
    mean_choice = mean(choice, na.rm = TRUE),
    sd_choice = sd(choice, na.rm = TRUE),
    .groups = "drop"
  )



## 4. Moderator Variables



sus_scores <- df_wide |> 
  select(starts_with("SUS_")) |>
  mutate(across(everything(), as.numeric))


# Reliability
psych::alpha(sus_scores, use = "pairwise")$total# alpha
psych::omega(sus_scores, use = "pairwise")$omega.tot # omega total


# Total score 

SUS_total <- rowMeans(sus_scores, na.rm=TRUE)

describe(SUS_total) # descriptives for SUS

hist(SUS_total)


## 5. Ceiling Effects


# By participant overall
ceiling_participants <- df_long |> 
  group_by(ppn) |> 
  summarise(
    n_trials = n(),
    n_valid = sum(!is.na(choice)),
    prop_high_effort = if_else(n_valid > 0, mean(choice, na.rm = TRUE), NA_real_)
  ) |> 
  filter(!is.na(prop_high_effort)) |>  # Remove participants with no valid data
  summarise(
    n_participants = n(),
    at_90 = sum(prop_high_effort >= 0.90),
    pct_90 = mean(prop_high_effort >= 0.90) * 100,
    at_95 = sum(prop_high_effort >= 0.95),
    pct_95 = mean(prop_high_effort >= 0.95) * 100,
    at_100 = sum(prop_high_effort == 1.00),
    pct_100 = mean(prop_high_effort == 1.00) * 100
  )
print(ceiling_participants)

# By participant and block
ceiling_by_block <- df_long |> 
  group_by(ppn, block) |> 
  summarise(
    n_trials = n(),
    n_valid = sum(!is.na(choice)),
    prop_high_effort = if_else(n_valid > 0, mean(choice, na.rm = TRUE), NA_real_),
    .groups = "drop"
  ) |> 
  filter(!is.na(prop_high_effort)) |>  # Remove participants with no valid data in that block
  group_by(block) |> 
  summarise(
    n_participants = n(),
    n_at_90 = sum(prop_high_effort >= 0.90),
    pct_at_90 = mean(prop_high_effort >= 0.90) * 100,
    n_at_95 = sum(prop_high_effort >= 0.95),
    pct_at_95 = mean(prop_high_effort >= 0.95) * 100,
    n_at_100 = sum(prop_high_effort == 1.00),
    pct_at_100 = mean(prop_high_effort == 1.00) * 100
  )
print(ceiling_by_block)


# 1. Compute each participant's high-effort proportion

participant_props <- df_long |> 
  group_by(ppn) |> 
  summarise(
    n_trials = n(),
    n_valid = sum(!is.na(choice)),
    prop_high_effort = if_else(n_valid > 0, mean(choice, na.rm = TRUE), NA_real_),
    .groups = "drop"
  ) |> 
  filter(!is.na(prop_high_effort))

# 2. Convert proportions to discrete 36-step values
plot_data <- participant_props |> 
  mutate(
    prop_disc = round(prop_high_effort * 36) / 36
  ) |> 
  count(prop_disc)

# 3. Define a threshold  for visualizing ceiling effects
threshold <- 0.90

# 4. Plot
ceiling_plot  <- ggplot(plot_data, aes(x = prop_disc, y = n)) +
  geom_col(fill = "steelblue") +
  scale_x_continuous(
    breaks = plot_data$prop_disc,
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_y_continuous(
    breaks = seq(0, max(plot_data$n), by = 10)
  ) +
  geom_vline(xintercept = threshold, linetype = "dashed", color = "red") +
  annotate(
    "text",
    x = threshold,
    y = max(plot_data$n) * 0.95,     # ensures text stays inside plot area
    label = "90% threshold",
    color = "red",
    hjust = -0.1
  ) +
  labs(
    x = "Proportion of high-effort choices (all 36 trials)",
    y = "Number of participants"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("figures/descriptives/figure1_ceiling_distribution.png", 
       plot = ceiling_plot,
       width = 7, 
       height = 5, dpi = 300)


# Descriptive
summary(participant_props$prop_high_effort)
quantile(participant_props$prop_high_effort, probs = c(0.25, 0.5, 0.75))

# How many chose high effort 100% of the time?
sum(participant_props$prop_high_effort == 1.0)
mean(participant_props$prop_high_effort == 1.0) * 100


# Standard deviation of choices (within-person)
within_person_variability <- df_long |> 
  group_by(ppn) |> 
  summarise(
    n_trials = n(),
    n_valid = sum(!is.na(choice)),
    sd_choice = if_else(n_valid > 1, sd(choice, na.rm = TRUE), NA_real_),
    var_choice = if_else(n_valid > 1, var(choice, na.rm = TRUE), NA_real_)
  ) |> 
  filter(!is.na(sd_choice)) |>  # Remove participants with insufficient data
  summarise(
    n_participants = n(),
    mean_sd = mean(sd_choice),
    median_sd = median(sd_choice),
    n_zero_var = sum(sd_choice == 0),
    pct_zero_var = mean(sd_choice == 0) * 100
  )
print(within_person_variability)

# Ceiling by experimental conditions
ceiling_by_group <- df_long |> 
  group_by(ppn, group) |> 
  summarise(
    n_trials = n(),
    n_valid = sum(!is.na(choice)),
    prop_high_effort = if_else(n_valid > 0, mean(choice, na.rm = TRUE), NA_real_),
    .groups = "drop"
  ) |> 
  filter(!is.na(prop_high_effort)) |>  # Remove participants with no valid data
  group_by(group) |> 
  summarise(
    n_participants = n(),
    n_at_90 = sum(prop_high_effort >= 0.90),
    pct_at_90 = mean(prop_high_effort >= 0.90) * 100,
    mean_prop = mean(prop_high_effort),
    sd_prop = sd(prop_high_effort),
    median_prop = median(prop_high_effort)
  )
print(ceiling_by_group)

# summary table
ceiling_summary <- df_long |> 
  group_by(ppn) |> 
  summarise(
    n_trials = n(),
    n_valid = sum(!is.na(choice)),
    prop = if_else(n_valid > 0, mean(choice, na.rm = TRUE), NA_real_),
    sd = if_else(n_valid > 1, sd(choice, na.rm = TRUE), NA_real_)
  ) |> 
  filter(!is.na(prop)) |>  # Remove participants with no valid data
  summarise(
    n_participants = n(),
    mean = mean(prop),
    sd = sd(prop),
    median = median(prop),
    q25 = quantile(prop, 0.25),
    q75 = quantile(prop, 0.75),
    min = min(prop),
    max = max(prop),
    n_90 = sum(prop >= 0.90),
    pct_90 = mean(prop >= 0.90) * 100,
    n_95 = sum(prop >= 0.95),
    pct_95 = mean(prop >= 0.95) * 100,
    n_100 = sum(prop == 1.00),
    pct_100 = mean(prop == 1.00) * 100,
    mean_within_sd = mean(sd, na.rm = TRUE),
    n_zero_var = sum(sd == 0, na.rm = TRUE),
    pct_zero_var = mean(sd == 0, na.rm = TRUE) * 100
  )
print(ceiling_summary)




# Visualizations

# a) Reward $\times$ Effort interaction



plot_a_data <- df_long |> 
  group_by(reward, effort) |> 
  summarise(p = mean(choice == 1, na.rm = TRUE), .groups = "drop")


reward_effort_plot <- plot_a_data |>
  ggplot(aes(x = reward, y = p, color = effort, group = effort)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  scale_color_manual(values = c(
    "40%" = "#E15759",
    "90%" = "#4E79A7"
  )) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    x = "Reward",
    y = "Proportion of High-Effort Choices",
    color = "Effort"
  ) +
  theme_bw()



ggsave("figures/descriptives/reward_effort_plot.png",  
       plot = reward_effort_plot,
       width = 7, height = 5, dpi = 300)




# b)  Target $\times$ Block $\times$ Group



plot_b_data <- df_long |>
  group_by(group, block, target) |>
  summarise(
    p_choice = mean(choice == 1, na.rm = TRUE),
    n       = sum(!is.na(choice)),
    .groups = "drop"
  )


target_block_group_plot <- plot_b_data |> 
  ggplot(aes(x = block,
             y = p_choice,
             color = target,
             group = target)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  facet_wrap(~ group, labeller = labeller(group = group_labels)) +
  labs(
    x     = "Block",
    y     = "Proportion of High-Effort Choices",
    color = "Target"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_manual(values = pal_target) +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.title    = element_blank()
  )

ggsave("figures/descriptives/target_block_group_plot.png",  
       plot = target_block_group_plot,
       width = 7, height = 5, dpi = 300)


# Adjusting y axis to zoom in on the effect

zoomed_target_block_group_plot <- plot_b_data |> 
  ggplot(aes(x = block,
             y = p_choice,
             color = target,
             group = target)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  facet_wrap(~ group, labeller = labeller(group = group_labels)) +
  labs(
    x     = "Block",
    y     = "Proportion of High-Effort Choices",
    color = "Target"
  ) +
  scale_y_continuous(limits = c(0.85, 1), 
                     breaks = seq(0.85, 1, 0.025)) +
  scale_color_manual(values = pal_target) +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.title    = element_blank()
  )

ggsave("figures/descriptives/zoomed_target_block_group_plot.png",
       width = 7, height = 5, dpi = 300)

message("Descriptives and plots created.")
