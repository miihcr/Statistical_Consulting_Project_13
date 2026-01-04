source(here::here("scripts", "_common.R"))
source_R()

df_raw <- readr::read_csv(pth_data_raw("data_2.csv"), show_col_types = FALSE)

out <- compute_moderators_pipeline(df_raw)

# Save enriched dataset
save_rds(out$data, pth_data_processed("data2_incl_moderation.rds"))

# Save tables
write_csv2(out$sus_reliability, pth_results_tables("01_sus_reliability.csv"))
write_csv2(out$cfa_fit_indices, pth_results_tables("01_cfa_fit_indices.csv"))
write_csv2(out$network_summary, pth_results_tables("01_network_summary.csv"))

# Save SEM plots (base plot device)
plot_sem_png(out$cfa_fits$one_factor,  pth_fig("sem-plots", "01_semplot_onefactor.png"))
plot_sem_png(out$cfa_fits$two_factor,  pth_fig("sem-plots", "01_semplot_twofactor.png"))
plot_sem_png(out$cfa_fits$minus_item2, pth_fig("sem-plots", "01_semplot_minus2.png"))
plot_sem_png(out$cfa_fits$minus_item4, pth_fig("sem-plots", "01_semplot_minus4.png"))
