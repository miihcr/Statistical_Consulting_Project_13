# modelling.R


make_ctrl <- function() {
  lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6))
}

fit_rq1_models <- function(df_long) {
  ctrl <- make_ctrl()
  
  m0 <- lme4::glmer(choice ~ 1 + (1 | ppn), data = df_long, family = binomial, control = ctrl)
  
  m1_with_class <- lme4::glmer(
    choice ~ target * group * block + reward * effort + school + (1 | ppn) + (1 | class),
    data = df_long, family = binomial, control = ctrl
  )
  
  m1_without_class <- lme4::glmer(
    choice ~ target * group * block + reward * effort + school + (1 | ppn),
    data = df_long, family = binomial, control = ctrl
  )
  
  m_rslopes <- lme4::glmer(
    choice ~ target * group * block + reward * effort + school + (1 + block | ppn),
    data = df_long, family = binomial, control = ctrl
  )
  
  m_full <- lme4::glmer(
    choice ~ target * group * block + reward * effort + school + (1 + block | ppn),
    data = df_long, family = binomial, control = ctrl
  )
  
  m_no_3way <- lme4::glmer(
    choice ~ target + group + block +
      target:group + target:block + group:block +
      reward * effort + school + (1 + block | ppn),
    data = df_long, family = binomial, control = ctrl
  )
  
  list(
    m0 = m0,
    m1_with_class = m1_with_class,
    m1_without_class = m1_without_class,
    m_rslopes = m_rslopes,
    m_full = m_full,
    m_no_3way = m_no_3way
  )
}

or_table <- function(model) {
  log_odds <- lme4::fixef(model)
  se <- sqrt(diag(stats::vcov(model)))
  p <- summary(model)$coefficients[, "Pr(>|z|)"]
  
  tibble::tibble(
    Predictor = names(log_odds),
    OR = exp(log_odds),
    SE = se,
    CI_Lower = exp(log_odds - 1.96 * se),
    CI_Upper = exp(log_odds + 1.96 * se),
    p_value = p
  )
}

performance_metrics <- function(model) {
  icc_val <- performance::icc(model)
  r2_vals <- performance::r2(model)
  tibble::tibble(
    ICC_adjusted = icc_val$ICC_adjusted,
    R2_marginal = r2_vals$R2_marginal,
    R2_conditional = r2_vals$R2_conditional
  )
}

dharma_diagnostics <- function(model, n = 1000) {
  sim <- DHARMa::simulateResiduals(fittedModel = model, n = n, plot = FALSE)
  mf <- stats::model.frame(model)
  
  sim_ppn <- DHARMa::recalculateResiduals(sim, group = mf$ppn)
  
  list(sim = sim, sim_ppn = sim_ppn)
}

fit_rq2_glmmtmb <- function(df_long) {
  # Baseline in glmmTMB (equivalent)
  baseline <- glmmTMB::glmmTMB(
    choice ~ target * group * block + reward * effort + school + (1 + block | ppn),
    data = df_long, family = binomial
  )
  
  # Susceptibility moderation
  m_sus_2a <- glmmTMB::glmmTMB(
    choice ~ target * group * block + susceptibility_c + reward * effort + school + (1 + block | ppn),
    data = df_long, family = binomial
  )
  
  m_sus_2b <- glmmTMB::glmmTMB(
    choice ~ target * group * block + group * block * susceptibility_c + reward * effort + school + (1 + block | ppn),
    data = df_long, family = binomial
  )
  
  # Cohesion moderation
  m_coh_3a <- glmmTMB::glmmTMB(
    choice ~ target * group * block + cohesion_capped_c + reward * effort + school + (1 + block | ppn),
    data = df_long, family = binomial
  )
  
  list(
    baseline = baseline,
    m_sus_2a = m_sus_2a,
    m_sus_2b = m_sus_2b,
    m_coh_3a = m_coh_3a
  )
}

plot_estimates_sjplot <- function(model, filename) {
  sjPlot::theme_set(sjPlot::theme_sjplot())
  p <- sjPlot::plot_model(model, type = "est", show.intercept = FALSE, vline.color = "red")
  ggsave2(filename, p)
  p
}