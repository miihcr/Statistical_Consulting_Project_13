source(here::here("scripts", "_common.R"))
source_R()

df_long <- read_rds(pth_data_processed("df_long.rds"))

# Tables
choice_tbl <- summarise_choice_rates(df_long)
write_csv2(choice_tbl, pth_results_tables("03_choice_rates.csv"))

ceiling_tbl <- ceiling_summary_table(df_long)
write_csv2(ceiling_tbl, pth_results_tables("03_ceiling_by_participant.csv"))

# Plots
p_re <- plot_reward_effort(df_long)
ggsave2(pth_fig("descriptives", "03_reward_effort_plot.png"), p_re)

p_tbg <- plot_target_block_group(df_long, zoom = FALSE)
ggsave2(pth_fig("descriptives", "03_target_block_group_plot.png"), p_tbg)

p_tbg_zoom <- plot_target_block_group(df_long, zoom = TRUE)
ggsave2(pth_fig("descriptives", "03_target_block_group_plot_zoomed.png"), p_tbg_zoom)
