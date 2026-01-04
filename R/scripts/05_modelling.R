source(here::here("scripts", "_common.R"))
source_R()

df_long <- read_rds(pth_data_processed("df_long.rds"))

# Ensure contrasts and reference levels (reproducible)
df_long <- df_long |>
  dplyr::mutate(
    group  = relevel(group,  ref = "control"),
    target = relevel(target, ref = "self"),
    effort = relevel(effort, ref = "40%"),
    block  = relevel(block,  ref = "pre")
  )
options(contrasts = c("contr.treatment", "contr.poly"))

# ---- RQ1 (lme4) --------------------------------------------------------------

mods <- fit_rq1_models(df_long)

# Model comparisons to tables
cmp1 <- as.data.frame(anova(mods$m1_without_class, mods$m1_with_class))
cmp2 <- as.data.frame(anova(mods$m1_without_class, mods$m_rslopes))
cmp3 <- as.data.frame(anova(mods$m_no_3way, mods$m_full))

write_csv2(cmp1, pth_results_tables("05_rq1_compare_class_re.csv"))
write_csv2(cmp2, pth_results_tables("05_rq1_compare_random_slopes.csv"))
write_csv2(cmp3, pth_results_tables("05_rq1_compare_3way.csv"))

final_model <- mods$m_full
save_rds(final_model, pth_results_models("05_rq1_final_model_lme4.rds"))

# OR table + performance
or <- or_table(final_model)
write_csv2(or, pth_results_tables("05_rq1_or_table.csv"))

perf <- performance_metrics(final_model)
write_csv2(perf, pth_results_tables("05_rq1_performance.csv"))

# Plot estimates
plot_estimates_sjplot(final_model, pth_fig("regression-plots", "05_rq1_model_estimates.png"))

# DHARMa diagnostics
diag <- dharma_diagnostics(final_model, n = 1000)
save_rds(diag, pth_results_objects("05_rq1_dharma_objects.rds"))

# Save diagnostic plots (base plotting)
w <- 7; h <- 5; dpi <- 300

png(pth_fig("diagnostics", "05_dharma_main.png"), width = w, height = h, units = "in", res = dpi)
plot(diag$sim)
dev.off()

png(pth_fig("diagnostics", "05_dharma_random_ppn.png"), width = w, height = h, units = "in", res = dpi)
plot(diag$sim_ppn)
dev.off()

# ---- RQ2 (glmmTMB moderation) ------------------------------------------------

rq2 <- fit_rq2_glmmtmb(df_long)

save_rds(rq2$baseline, pth_results_models("05_rq2_baseline_glmmtmb.rds"))
save_rds(rq2$m_sus_2b, pth_results_models("05_rq2_sus_2b_glmmtmb.rds"))
save_rds(rq2$m_coh_3a, pth_results_models("05_rq2_coh_3a_glmmtmb.rds"))

# Compare susceptibility models (2a vs 2b)
sus_cmp <- as.data.frame(anova(rq2$m_sus_2a, rq2$m_sus_2b))
write_csv2(sus_cmp, pth_results_tables("05_rq2_sus_model_compare.csv"))

# Plots for moderation models (estimates)
plot_estimates_sjplot(rq2$m_sus_2b, pth_fig("regression-plots", "05_rq2_sus_2b_estimates.png"))
plot_estimates_sjplot(rq2$m_coh_3a, pth_fig("regression-plots", "05_rq2_coh_3a_estimates.png"))

# DHARMa diagnostics for glmmTMB models
diag_sus <- dharma_diagnostics(rq2$m_sus_2b, n = 1000)
save_rds(diag_sus, pth_results_objects("05_rq2_sus_dharma_objects.rds"))

png(pth_fig("diagnostics", "05_rq2_sus_dharma_main.png"), width = w, height = h, units = "in", res = dpi)
plot(diag_sus$sim)
dev.off()

diag_coh <- dharma_diagnostics(rq2$m_coh_3a, n = 1000)
save_rds(diag_coh, pth_results_objects("05_rq2_coh_dharma_objects.rds"))

png(pth_fig("diagnostics", "05_rq2_coh_dharma_main.png"), width = w, height = h, units = "in", res = dpi)
plot(diag_coh$sim)
dev.off()
