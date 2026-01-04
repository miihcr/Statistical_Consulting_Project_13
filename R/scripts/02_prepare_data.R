source(here::here("scripts", "_common.R"))
source_R()

df_wide <- read_rds(pth_data_processed("data2_incl_moderation.rds"))

out <- make_long(df_wide)

save_rds(out$df_wide, pth_data_processed("df_wide.rds"))
save_rds(out$df_long, pth_data_processed("df_long.rds"))

# Basic QC outputs
qc <- tibble::tibble(
  n_wide = nrow(out$df_wide),
  n_long = nrow(out$df_long),
  missing_choice = sum(is.na(out$df_long$choice))
)
write_csv2(qc, pth_results_tables("02_qc_counts.csv"))
