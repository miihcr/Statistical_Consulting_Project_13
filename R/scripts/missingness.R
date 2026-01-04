# missingness.R



missingness_outputs <- function(df_wide) {
  # Returns plots (ggplot objects) and summaries
  prop <- naniar::prop_miss(df_wide)
  miss_var <- naniar::miss_var_summary(df_wide)
  miss_case <- naniar::miss_case_summary(df_wide)
  
  p1 <- naniar::vis_miss(df_wide, sort_miss = TRUE)
  p2 <- naniar::gg_miss_var(df_wide)
  
  df_nab <- naniar::bind_shadow(df_wide)
  # Example: check nom_like missingness vs SUS_1
  p3 <- ggplot(df_nab, aes(x = nom_like_NA, y = SUS_1)) + geom_boxplot()
  
  list(
    prop_miss = prop,
    miss_var_summary = miss_var,
    miss_case_summary = miss_case,
    plot_vis_miss = p1,
    plot_gg_miss_var = p2,
    plot_box_nomlike_sus1 = p3
  )
}