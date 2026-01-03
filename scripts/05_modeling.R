# 05_modeling


## --- Statistical Modeling --- ##


# Load data


df_wide <- readRDS(here::here("data","processed","df_wide.rds"))
df_long <- readRDS(here::here("data","processed","df_long.rds"))

df_long <- df_long |>
  mutate(
    group  = relevel(group,  ref = "control"),
    target = relevel(target, ref = "self"),
    effort = relevel(effort, ref = "40%"),
    block  = relevel(block,  ref = "pre")
  )

options(contrasts = c("contr.treatment", "contr.poly"))

ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6))

dir.create("figures/regression-plots", recursive = TRUE, showWarnings = FALSE)
dir.create("figures/diagnostics", recursive = TRUE, showWarnings = FALSE)


# Statistical Analysis

## Research Question 1

## 1. Model Selection

  ### Step 1: Test whether classroom random intercept is needed
  
  

# Baseline model (intercept only)

m0 <- glmer(
  choice ~ 1 + (1 | ppn),  
  data = df_long, 
  family = binomial, 
  control = ctrl
)


m1_with_class <- glmer(
  choice ~ target * group * block + reward * effort + school +
    (1 | ppn) + (1 | class),
  data = df_long, 
  family = binomial, 
  control = ctrl
)


m1_without_class <- glmer(
  choice ~ target * group * block + reward * effort + school +
    (1 | ppn),
  data = df_long, 
  family = binomial, 
  control = ctrl
)


# Compare models
print(anova(m1_without_class, m1_with_class))

# Check for singularity

print(VarCorr(m1_with_class))



### Step 2: Test whether random slopes for block is needed



m_rslopes <- glmer(
  choice ~ target*group*block + reward*effort + school +
    (1 + block | ppn),
  data = df_long, family = binomial, control = ctrl
)

# Compare models

anova(m1_without_class, m_rslopes)

# Check for singularity

print(VarCorr(m_rslopes))


### Step 3: Test 3-way interaction (Target $\times$ Group $\times$ Block)



# Full model with three-way interaction
m_full <- glmer(
  choice ~ target * group * block + reward * effort + school +
    (1 + block | ppn),
  data = df_long, 
  family = binomial, 
  control = ctrl
)

# Model without three-way interaction (only two-way)
m_no_3way <- glmer(
  choice ~ target + group + block + 
    target:group + target:block + group:block +  
    reward * effort + school +
    (1 + block | ppn),
  data = df_long, 
  family = binomial, 
  control = ctrl
)



# Compare models
anova(m_no_3way, m_full)



### Final Model Selection for RQ1


final_model <- m_full

# Check singularity
isSingular(final_model)

# Print variance components

print(VarCorr(final_model))

# Compare with baseline

print(anova(m0, final_model))




## 2. Model Summary and Effect Sizes


# Model summary

print(summary(final_model))

# Odds Ratios with 95% CIs

# Extract coefficients and SEs
log_odds <- lme4::fixef(final_model)
se <- sqrt(diag(vcov(final_model)))

# Calculate ORs and CIs
or_table <- data.frame(
  Predictor = names(log_odds),
  OR = exp(log_odds),
  SE = se,
  CI_Lower = exp(log_odds - 1.96 * se),
  CI_Upper = exp(log_odds + 1.96 * se),
  p_value = summary(final_model)$coefficients[, "Pr(>|z|)"]
)



# Round for display
or_table_display <- or_table
or_table_display[, c("OR", "SE", "CI_Lower", "CI_Upper")] <- 
  round(or_table_display[, c("OR", "SE", "CI_Lower", "CI_Upper")], 3)
or_table_display$p_value <- round(or_table_display$p_value, 4)

print(or_table_display)


# Model estimates plot
theme_set(theme_sjplot())
sjPlot::plot_model(m_full,
                   order.terms = c( 21, 22, 23, 24, # 3-way interactions 
                                    17, 18,           # Group × Block interactions 
                                    15, 16,           # Target × Block interactions 
                                    11, 12, 13, 14,   # Target × Group interactions 
                                    5,                # blockpost 
                                    3, 4,             # group main effects 
                                    1, 2,             # target main effects 
                                    6, 7, 8,          # reward and effort main effects 
                                    19, 20,           # reward × effort interactions 
                                    9, 10             # school effects 
                   ),            # school effects)
                   vline.color = "red")


ggsave("figures/regression-plots/model-estimates.png",
       width = 7, height = 5, dpi = 300)



## 3. Model Performance Metrics


# ICC
icc_val <- performance::icc(final_model)
round(icc_val$ICC_adjusted, 3)

# R-squared
r2_vals <- performance::r2(final_model)
round(r2_vals$R2_marginal, 3)
round(r2_vals$R2_conditional, 3)



## 4. Post-hoc Analyses

### 4. 1. Reward $\times$ Effort interaction



#  Predicted probabilities 

emm_re <- emmeans(final_model, ~ reward * effort, type = "response")

# Simple slopes: Effect of effort at each reward level
contrast(emm_re, "pairwise", by = "reward", adjust = "holm")

# Simple slopes: Effect of reward at each effort level
contrast(emm_re, "pairwise", by = "effort", adjust = "holm")

# Visualization

gg_re <- ggpredict(final_model, terms = c("reward", "effort"))

plot_re <- plot(gg_re) +
  labs(
    title = "Predicted Probability of High-Effort Choice",
    y = "P(High-Effort Choice)",
    x = "Reward (points)"
  ) +
  theme_bw() +
  scale_y_continuous(limits = c(1, 0))


ggsave("figures/regression-plots/reward_effort_interaction.png",  
       plot = plot_re,
       width = 7, height = 5, dpi = 300)



### 4. 2. Target $\times$ Group interaction (Exploratory)

# Estimated marginal means
emm_tg <- emmeans(final_model, ~ target * group, type = "response")


# Simple effects: Target differences within each group

contrast(emm_tg, "pairwise", by = "group", adjust = "holm")

# Simple effects: Group differences within each target

contrast(emm_tg, "pairwise", by = "target", adjust = "holm")

# Visualization

gg_tg <- ggpredict(final_model, terms = c("target", "group"))

plot_tg <- plot(gg_tg) +
  labs(
    title = "Target × Group Interaction (Exploratory)",
    subtitle = "Differential effects across targets, particularly for prosocial",
    y = "Predicted P(High-Effort Choice)",
    x = "Social Target",
    color = "Group"
  ) +
  theme_bw() +
  scale_y_continuous(limits = c(1, 0))

ggsave("figures/regression-plots/target_group_interaction.png",  
       plot = plot_tg,
       width = 7, height = 5, dpi = 300)




### 4. 3. Block × Group × Target: Primary Hypothesis - NULL finding



# Estimated marginal means for full three-way
emm_full <- emmeans(final_model, ~ target * group * block,
                    type = "response")



# Pre-to-post changes within each Target × Group combination

prepost <- contrast(
  emm_full,
  method = "pairwise",
  by = c("target", "group")
)

print(as.data.frame(prepost))

# Difference-in-differences: Compare change scores across groups

did <- contrast(
  emm_full,
  interaction = c("pairwise", "pairwise"),
  by = "target",
  adjust = "holm"
)
print(as.data.frame(did))

# Visualization showing ceiling effect

gg_3way <- ggpredict(final_model, terms = c("block", "group", "target"))

plot_3way <- plot(gg_3way) +
  labs(
    title = "Target × Group × Block: Primary Hypothesis Test",
    subtitle = "No differential effects detected (ceiling effects prevent detection)",
    y = "Predicted P(High-Effort Choice)",
    x = "Block",
    color = "Group"
  ) +
  facet_wrap(~ facet, ncol = 3) +
  theme_bw() +
  scale_y_continuous(limits = c(0.85, 1.0)) +  # Show the ceiling
  geom_hline(yintercept = 0.92, linetype = "dashed", color = "red", alpha = 0.5) +
  annotate("text", x = 1.5, y = 0.86, 
           label = "Red line = observed mean (92.4%)", 
           size = 3, color = "red")



ggsave("figures/regression-plots/three_way_interaction_primary_hypothesis.png",  
       plot = plot_3way,
       width = 7, height = 5, dpi = 300)


## 5. Final Model Diagnostics



#  Simulate DHARMa residuals

sim <- simulateResiduals(
  fittedModel = final_model,
  n = 1000,
  plot = FALSE
)


# Main diagnostic plots

plot(sim) # QQ + residuals vs fitted

plotQQunif(sim)  # QQ only
plotResiduals(sim) # residuals vs predicted only

# Statistical tests

testUniformity(sim)      # Should be non-significant
testDispersion(sim)      # Should be non-significant
testOutliers(sim)     # OK if significant for GLMMs, consider type="bootstrap" if significant

# Predictor-specific residual checks

mf <- model.frame(final_model) # because of NA values 


plotResiduals(sim, mf$target, main = "Residuals vs. Target")
plotResiduals(sim, mf$group, main = "Residuals vs. Group")
plotResiduals(sim, mf$block, main = "Residuals vs. Block")
plotResiduals(sim, mf$reward, main = "Residuals vs. Reward")
plotResiduals(sim, mf$effort, main = "Residuals vs. Effort")
plotResiduals(sim, mf$school, main = "Residuals vs. School")


# Grouped residuals (random-effects diagnostic)
sim_ppn <- recalculateResiduals(sim, group = mf$ppn)
plot(sim_ppn)
testDispersion(sim_ppn)


performance::icc(final_model)

performance::r2(final_model)




### Save Plots for report


# Save plots

w <- 7
h <- 5
dpi <- 300

# Main dharma plot

png("figures/diagnostics/dharma_main.png", width = w, height = h, units = "in", res = dpi)
plot(sim)
dev.off()


png("figures/diagnostics/dharma_random.png", width = w, height = h, units = "in", res = dpi)
plot(sim_ppn)
dev.off()


png("figures/diagnostics/lattice_random.png", width = w, height = h, units = "in", res = dpi)
randoms1 <- ranef(final_model)
lattice::qqmath(randoms1)
dev.off()




## Research Question 2: Moderation Analysis


# Fit baseline model in glmmTMB
baseline_glmmTMB <- glmmTMB(
  choice ~ target * group * block + reward * effort + 
    school +
    (1 + block | ppn),
  data = df_long,
  family = binomial
)

summary(baseline_glmmTMB)
# summary(final_model) # Models are identical



### Social Susceptibility Moderation (H4)


# Model 2a: Base model with susceptibility main effect only
m_suscept_2a <- glmmTMB(
  choice ~ target * group * block + susceptibility_c + 
    reward * effort + school +
    (1 + block | ppn),
  data = df_long, 
  family = binomial
)

# Model 2b: Add Group × Block × Susceptibility


m_suscept_2b <- glmmTMB(
  choice ~ target * group * block + 
    group * block * susceptibility_c + 
    reward * effort + school +
    (1 + block | ppn),
  data = df_long, 
  family = binomial
)


# Model 2c: Add three-way interactions with target


m_suscept_2c <- glmmTMB(
  choice ~ (group * block * target) + 
    (group * block * susceptibility_c) +
    (group * target * susceptibility_c) + 
    (block * target * susceptibility_c) +
    reward * effort + school +
    (1 + block | ppn),
  data = df_long, 
  family = binomial
)


# Model 2d: Full four-way interaction

m_suscept_2d <- glmmTMB(
  choice ~ group * block * target * susceptibility_c + 
    reward * effort + school +
    (1 + block | ppn),
  data = df_long, 
  family = binomial
)


# Compare models
print(anova(m_suscept_2a, m_suscept_2b, m_suscept_2c, m_suscept_2d))

m_suscept_final <- m_suscept_2b


#### Model Summary


print(summary(m_suscept_final))

# Extract key coefficients

suscept_coefs <- summary(m_suscept_final)$coefficients$cond
suscept_rows <- grep("susceptibility", rownames(suscept_coefs), ignore.case = TRUE)


print(round(suscept_coefs[suscept_rows, ],4))

sjPlot::tab_model(m_suscept_2b)



#### Model Diagnostics


#  Simulate DHARMa residuals

sim_sus <- simulateResiduals(
  fittedModel = m_suscept_final,
  n = 1000,
  plot = FALSE
)


# Main diagnostic plots

plot(sim_sus) # QQ + residuals vs fitted

plotQQunif(sim_sus)  # QQ only
plotResiduals(sim_sus) # residuals vs predicted only

# Statistical tests

testUniformity(sim_sus)      # Should be non-significant
testDispersion(sim_sus)      # Should be non-significant
testOutliers(sim_sus)     # OK if significant for GLMMs, consider type="bootstrap" if significant

# Predictor-specific residual checks

mf_sus <- model.frame(m_suscept_final) # because of NA values 


plotResiduals(sim_sus, mf_sus$target, main = "Residuals vs. Target")
plotResiduals(sim_sus, mf_sus$group, main = "Residuals vs. Group")
plotResiduals(sim_sus, mf_sus$block, main = "Residuals vs. Block")
plotResiduals(sim_sus, mf_sus$reward, main = "Residuals vs. Reward")
plotResiduals(sim_sus, mf_sus$effort, main = "Residuals vs. Effort")
plotResiduals(sim_sus, mf_sus$school, main = "Residuals vs. School")


# Grouped residuals (random-effects diagnostic)
sim_ppn_sus <- recalculateResiduals(sim_sus, group = mf_sus$ppn)
plot(sim_ppn_sus)
testDispersion(sim_ppn_sus)


performance::icc(m_suscept_final)

performance::r2(m_suscept_final)


### Classroom Cohesion Moderation (H5)

#### Directed Density


# use directed
df_long$cohesion_z <- scale(df_long$cohesion_directed)[, 1]
# df_long$cohesion_z <- scale(df_long$cohesion_recip)[, 1]

m_cohesion_test_no_class <- glmmTMB(
  choice ~ target * group * block + cohesion_z + 
    reward * effort + school +
    (1 + block | ppn),
  data = df_long, 
  family = binomial
)

m_cohesion_test_with_class <- glmmTMB(
  choice ~ target * group * block + cohesion_z  + 
    reward * effort + school +
    (1 + block | ppn) + (1 | class),
  data = df_long, 
  family = binomial
)

print(anova(m_cohesion_test_no_class, m_cohesion_test_with_class))


# Model 3a: Base model with cohesion main effect

m_cohesion_3a <- glmmTMB(
  choice ~ target * group * block + cohesion_z 
  + reward * effort + school +
    (1 + block | ppn),
  data = df_long, 
  family = binomial
)

# Model 3b: Add Group × Block × Cohesion

m_cohesion_3b <- glmmTMB(
  choice ~ target * group * block + 
    group * block * cohesion_z + 
    reward * effort + school +
    (1 + block | ppn),
  data = df_long, 
  family = binomial
)

# Model 3c: Add three-way interactions with target

m_cohesion_3c <- glmmTMB(
  choice ~ (group * block * target) + 
    (group * block * cohesion_z) +
    (group * target * cohesion_z) + 
    (block * target * cohesion_z) +
    reward * effort + school +
    (1 + block | ppn),
  data = df_long, 
  family = binomial
)

# Model 3d: Full four-way interaction

m_cohesion_3d <- glmmTMB(
  choice ~ group * block * target * cohesion_z + 
    reward * effort + school +
    (1 + block | ppn),
  data = df_long, 
  family = binomial
)


print(anova(m_cohesion_3a, m_cohesion_3b, m_cohesion_3c, m_cohesion_3d))

summary(m_cohesion_3c)
sjPlot::tab_model(m_cohesion_3c)


##### Model Diagnostics


#  Simulate DHARMa residuals

sim_coh <- simulateResiduals(
  fittedModel = m_cohesion_3c,
  n = 1000,
  plot = FALSE
)


# Main diagnostic plots

plot(sim_coh) # QQ + residuals vs fitted

plotQQunif(sim_coh)  # QQ only
plotResiduals(sim_coh) # residuals vs predicted only

# Statistical tests

testUniformity(sim_coh)      # Should be non-significant
testDispersion(sim_coh)      # Should be non-significant
testOutliers(sim_coh)     # OK if significant for GLMMs, consider type="bootstrap" if significant

# Predictor-specific residual checks

mf_coh <- model.frame(m_cohesion_3b) # because of NA values 


plotResiduals(sim_coh, mf_coh$target_coh, main = "Residuals vs. Target")
plotResiduals(sim_coh, mf_coh$group_coh, main = "Residuals vs. Group")
plotResiduals(sim_coh, mf_coh$block, main = "Residuals vs. Block")
plotResiduals(sim_coh, mf_coh$reward, main = "Residuals vs. Reward")
plotResiduals(sim_coh, mf_coh$effort, main = "Residuals vs. Effort")
plotResiduals(sim_coh, mf_coh$school, main = "Residuals vs. School")


# Grouped residuals (random-effects diagnostic)
sim_ppn_coh <- recalculateResiduals(sim_coh, group = mf_coh$ppn)
plot(sim_ppn_coh)
testDispersion(sim_ppn_coh)


performance::icc(m_cohesion_3c)

performance::r2(m_cohesion_3c)


# Model estimates plots (only important ones)

theme_set(theme_sjplot())

terms_cohesion_keep <- c(
  # 3-way interactions 
  "grouppositive_norm:blockpost:cohesion_z",
  "groupnegative_norm:blockpost:cohesion_z",
  "grouppositive_norm:targetclimate:cohesion_z",
  "groupnegative_norm:targetclimate:cohesion_z",
  "grouppositive_norm:targetprosocial:cohesion_z",
  "groupnegative_norm:targetprosocial:cohesion_z",
  "blockpost:targetclimate:cohesion_z",
  "blockpost:targetprosocial:cohesion_z",
  
  # 2-way interactions
  "grouppositive_norm:cohesion_z",
  "groupnegative_norm:cohesion_z",
  "blockpost:cohesion_z",
  "targetclimate:cohesion_z",
  "targetprosocial:cohesion_z",
  
  # main effect
  
  "cohesion_z"
  
)

p_cohesion_3c <- sjPlot::plot_model(
  m_cohesion_3c,
  type = "est",
  terms = terms_cohesion_keep,   # <- this is what removes stuff
  sort.est = FALSE,              # <- respects your order above
  show.intercept = FALSE,
  vline.color = "red"
)

p_cohesion_3c

ggsave("figures/regression-plots/m-cohesion-3c.png",
       plot = p_cohesion_3c, width = 7, height = 5, dpi = 300)

theme_set(theme_sjplot())

terms_suscept_keep <- c(
  # 3-way interactions 
  "grouppositive_norm:blockpost:susceptibility_c",
  "groupnegative_norm:blockpost:susceptibility_c",
  
  # 2-way interactions 
  
  "grouppositive_norm:susceptibility_c",
  "groupnegative_norm:susceptibility_c",
  "blockpost:susceptibility_c",
  
  # main effects 
  
  "susceptibility_c"
  
)

p_suscept_2b <- sjPlot::plot_model(
  m_suscept_2b,
  type = "est",
  terms = terms_suscept_keep,
  sort.est = FALSE,
  show.intercept = FALSE,
  vline.color = "red"
)

ggsave("figures/regression-plots/m-suscept-2b.png",
       plot = p_suscept_2b, width = 7, height = 5, dpi = 300)



message("Modeling and moderation analysis completed.")
