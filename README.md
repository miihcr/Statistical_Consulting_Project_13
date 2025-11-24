Statistical Consulting Project 13

This repository contains the full reproducible analysis pipeline for Statistical Consulting Project 13. 


Project Structure

Statistical_Consulting_Project_13/
│
├── data/
│   ├── raw/               # Original provided datasets
│   ├── processed/         # Cleaned & transformed data saved as RDS
│
├── figures/
│   ├── descriptives/      # All descriptive plots
│   ├── sem-plots/         # CFA visualisations
│   ├── missingness/       # Missingness diagnostics
│   ├── regression-plots/  # Model estimates, interactions
│   └── diagnostics/       # DHARMa diagnostics
│
├── scripts/
│   ├── 00_setup.R               # pipeline — runs everything in sequence
│   ├── _common.R                # Shared utilities, package loading, helpers
│   ├── 01_compute_moderators.R  # SUS scores, CFA, networks, cohesion
│   ├── 02_prepare_data.R        # Preprocessing, recoding, wide → long
│   ├── 03_descriptives.R        # Descriptive stats + key plots
│   ├── 04_missingness.R         # Missing data diagnostics
│   └── 05_modeling.R            # Main GLMM models + moderation
│
└── README.md


How to Run the Full Pipeline
1. Install required R packages

All packages used throughout the analysis are loaded via scripts/_common.R.
You can install missing packages automatically by running:

source("scripts/_common.R")


If a package is not installed, the script will stop and tell you which one to install.

2. Run the entire analysis

Simply execute:

source("scripts/00_setup.R")


This will:

Compute social susceptibility & cohesion moderators

Prepare the dataset (wide → long)

Generate all descriptive statistics and plots

Conduct missing data analyses

Fit all GLMM models (RQ1 & RQ2)

Save all figures and diagnostics into the figures/ folder

No manual steps required.

📊 Overview of Analysis Steps
1. Social Susceptibility & Social Cohesion (01_compute_moderators.R)

Reliability analyses (Cronbach’s α, McDonald’s ω)

CFA: 1-factor vs 2-factor SUS structure

SEM path diagrams saved to figures/sem-plots/

Classroom social networks (directed & reciprocal)

Cohesion metrics per class:

Density

Reciprocity

Transitivity

Path lengths

Component size

Outputs saved to:
data/processed/data2_incl_moderation.rds

2. Data Preparation (02_prepare_data.R)

Factor recoding

Extract trial structure (block × target × reward × effort)

Convert wide → long

Clean and recode the binary outcome

Attach moderator variables

Outputs saved to:
data/processed/df_wide.rds
data/processed/df_long.rds

3. Descriptive Statistics (03_descriptives.R)

Generates:

Reward × Effort plot

Target × Block × Group plot

Ceiling distribution plot

Summary tables of response patterns

Figures saved to:
figures/descriptives/

4. Missingness (04_missingness.R)

Includes:

Missing variable summaries

Missing case summaries

Missingness heatmaps

Boxplots for MAR / MCAR exploration

Figures saved to:
figures/missingness/

5. Statistical Modeling (05_modeling.R)
Research Question 1 (Main Effects of Manipulation)

Mixed-effects logistic regression (glmer)

Random slopes vs random intercepts comparisons

Test of Target × Group × Block

Odds ratios & confidence intervals

ICC & R²

Post-hoc contrasts (EMMs)

Research Question 2 (Moderation)

Moderation by:

Social Susceptibility

Classroom Cohesion (directed and reciprocal density)

Fitted using glmmTMB

Full model comparisons (2a–2d, 3a–3d, 4a–4d)

DHARMa diagnostics

Figures saved to:
figures/regression-plots/
figures/diagnostics/

Reproducibility

The workflow is fully scripted:

No manual modifications needed

Figures are automatically regenerated

Outputs are deterministic given identical raw data

All random-effects models use default seeds unless specified.

👥 Authors & Contact

This repository was developed as part of:

Statistical Consulting Project 13
Department of Psychology, Leiden University

For questions or requests, please contact:
[Your Name]
Email: your.email@university.nl
