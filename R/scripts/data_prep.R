# data_prep.R

make_long <- function(df_wide) {
  df_wide <- df_wide |>
    dplyr::mutate(
      ppn = as.factor(ppn),
      school = factor(school, levels = c(1, 2, 3), labels = c("1_loc1", "1_loc2", "2")),
      class = as.factor(class),
      group = factor(group, levels = c(1, 2, 3), labels = c("control", "positive_norm", "negative_norm")),
      school_combined = dplyr::case_when(
        school %in% c("1_loc1", "1_loc2") ~ "School_1",
        school == "2" ~ "School_2",
        TRUE ~ as.character(school)
      ),
      cohesion_capped = as.numeric(cohesion_capped),
      cohesion_capped_c = as.numeric(scale(cohesion_capped, center = TRUE, scale = FALSE))
    )
  
  trial_pattern <- "^(X)?[12]_(SELF|CLIMATE|OTHERS)_(2|6|10)(easy|hard)(40|90)$"
  trial_cols <- names(dplyr::select(df_wide, dplyr::matches(trial_pattern)))
  
  df_long <- df_wide |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(trial_cols),
      names_to = "trial",
      values_to = "choice_raw"
    ) |>
    dplyr::mutate(
      trial_clean = stringr::str_remove(trial, "^X"),
      block_num   = as.integer(stringr::str_extract(trial_clean, "^[12]")),
      target_raw  = stringr::str_extract(trial_clean, "(SELF|CLIMATE|OTHERS)"),
      reward_num  = as.integer(stringr::str_extract(trial_clean, "(?<=_)\\d{1,2}(?=(easy|hard))")),
      effort_raw  = as.integer(stringr::str_extract(trial_clean, "(40|90)$")),
      block  = factor(block_num, levels = c(1, 2), labels = c("pre", "post")),
      target = factor(target_raw, levels = c("SELF", "CLIMATE", "OTHERS"), labels = c("self", "climate", "prosocial")),
      effort = factor(effort_raw, levels = c(40, 90), labels = c("40%", "90%")),
      reward = factor(reward_num, levels = c(2, 6, 10), labels = c("2 points", "6 points", "10 points")),
      choice = dplyr::case_when(
        choice_raw == 1 ~ 1L,
        choice_raw == 2 ~ 0L,
        TRUE ~ NA_integer_
      ),
      group  = relevel(group,  ref = "control"),
      target = relevel(target, ref = "self"),
      effort = relevel(effort, ref = "40%"),
      block  = relevel(block,  ref = "pre")
    ) |>
    dplyr::select(
      ppn, school, school_combined, class, group,
      trial = trial_clean, block, target,
      reward, effort, choice,
      susceptibility, susceptibility_c,
      cohesion_capped, cohesion_capped_c,
      avg_outdegree, avg_outdegree_c
    )
  
  list(df_wide = df_wide, df_long = df_long)
}