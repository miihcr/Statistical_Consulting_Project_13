# Statistical Consulting Project 13



This repository contains the analysis pipeline for Statistical Consulting Project 13.


---


## Project Structure


```

Statistical_Consulting_Project_13/

│

├── data/

│   ├── raw/               # Original provided datasets

│   └── processed/         # Cleaned & transformed data saved as RDS

│

├── figures/

│   ├── descriptives/      # All descriptive plots

│   ├── sem-plots/         # CFA visualisations

│   ├── missingness/       # Missingness diagnostics

│   ├── regression-plots/  # Model estimates & interactions

│   └── diagnostics/       # DHARMa diagnostics

│

├── scripts/

│   ├── 00_setup.R               # Pipeline — runs everything in sequence

│   ├── _common.R                # Shared utilities, package loading, helpers

│   ├── 01_compute_moderators.R  # SUS scores, CFA, networks, cohesion

│   ├── 02_prepare_data.R        # Preprocessing, recoding, wide → long

│   ├── 03_descriptives.R        # Descriptive stats + key plots

│   ├── 04_missingness.R         # Missing data diagnostics

│   └── 05_modeling.R            # Main GLMM models + moderation

│

└── README.md

```

---


##  How to Run the Full Pipeline



### 1. Install required R packages

All packages are loaded through `_common.R`.

Missing packages will trigger an informative error message.

```r

source("scripts/_common.R")

```

---


### 2. Run the entire analysis

Run:


```r

source("scripts/00_setup.R")

```

This will automatically perform:


* computation of social susceptibility & cohesion moderators

* data preparation (wide → long)

* descriptive statistics and visualisation

* missingness analysis

* mixed-effects modelling and moderation

* exporting all figures to the `figures/` directory


---

## Analysis Overview

### 1. Moderators (`01_compute_moderators.R`)

* Computes social susceptibility scales

* Reliability (α, ω total)

* CFA (1-factor vs. 2-factor) + SEM path plots

* Classroom social networks

* Cohesion metrics (density, reciprocity, clustering, distances)

* Saves processed moderator dataset

### 2. Data Preparation (`02_prepare_data.R`)

* Factor recoding

* Trial parsing from column names

* Wide → long conversion

* Outcome cleaning (binary effort choice)

* Attaches moderators

### 3. Descriptives (`03_descriptives.R`)

* Summary statistics

* Reward × Effort plot

* Group × Target × Block plot

* Ceiling effect analysis and plots
* 

### 4. Missingness (`04_missingness.R`)

* Missing data maps

* Variable-level \& case-level summaries

* Boxplots to inspect MAR/MNAR patterns


### 5. Modeling (`05_modeling.R`)

* Mixed-effects logistic regression (GLMM)

* Random-effect structure testing

* Primary hypothesis testing

* Post-hoc contrasts via EMMs

* Moderation (susceptibility, cohesion)

* Full DHARMa diagnostics

---

## Authors

Milena Costa and Klāvs Kalvenieks


This repository was developed as part of:

Statistical Consulting Project 13
Faculty of Science, Leiden University


---



