source(here::here("scripts", "_common.R"))
source_R()

df_wide <- readr::read_csv(pth_data_raw("data_2.csv"), show_col_types = FALSE)

out <- missingness_outputs(df_wide)

# Save summaries
write_csv2(out$miss_var_summary,  pth_results_tables("04_miss_var_summary.csv"))
write_csv2(out$miss_case_summary, pth_results_tables("04_miss_case_summary.csv"))
write_csv2(tibble::tibble(prop_miss = out$prop_miss), pth_results_tables("04_prop_miss.csv"))

# Save plots
ggsave2(pth_fig("missingness", "04_vis_miss.png"), out$plot_vis_miss)
ggsave2(pth_fig("missingness", "04_gg_miss_var.png"), out$plot_gg_miss_var)
ggsave2(pth_fig("missingness", "04_box_nomlike_vs_sus1.png"), out$plot_box_nomlike_sus1)
