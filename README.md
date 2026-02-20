# Statistical Consulting Project 13



This repository contains the analysis pipeline for Statistical Consulting Project 13 at Leiden University.

All analyses are documented and executable via R Markdown (.Rmd) files in the `analysis/` folder.

Raw data are not included in this repository due to access restrictions.
Users must manually place the raw data files in `data/raw/`, using the exact file names as referenced in the notebooks (e.g., `data_2.csv`).

The `data/processed/` directory is generated locally when running the notebooks and is not tracked in Git.

---


## Project Structure


```

Statistical_Consulting_Project_13/
│
├── data/
│   ├── raw/                   # Raw data (not included; add manually)
│   └── processed/             # Saved processed objects (RDS)
│
├── figures/
│   ├── descriptives/          # Descriptive plots
│   ├── sem-plots/             # CFA visualisations (semPlot)
│   ├── missingness/           # Missingness diagnostics
│   ├── regression-plots/      # Model estimates & interaction plots
│   └── diagnostics/           # DHARMa diagnostics
│
├── scripts/
│   └── _common.R              # Shared utilities, package loading, helper functions
│
├── analysis/                  # Rmd notebooks (main analysis, with annotations)
│   ├── 00_moderators.Rmd
│   ├── 01_data_preparation.Rmd
│   ├── 02_descriptives.Rmd
│   ├── 03_missingness_raw.Rmd
│   └── 04_statistical_modeling.Rmd
│
├── .Rprofile                  # Project startup file (loads common utilities)
└── README.md


```

---


##  How to Run the Analysis


**Note:** All required R packages are loaded automatically via `scripts/_common.R`.  
No manual setup is required; missing packages will trigger an informative error.


### 1. Open the project in RStudio

Open the project folder as your working directory (project root).

### 2. Run the notebooks 

Run/knit the R Markdown files in this order:

1. **`analysis/00_moderators.Rmd`**  
   Computes social susceptibility measures, conducts CFA models, and derives classroom network cohesion metrics.

2. **`analysis/01_data_preparation.Rmd`**  
   Prepares the analysis dataset, including factor recoding and wide-to-long reshaping.

3. **`analysis/02_descriptives.Rmd`**  
   Produces descriptive statistics and key figures.

4. **`analysis/03_missingness_raw.Rmd`**  
   Performs missing-data exploration and generates diagnostic figures.

5. **`analysis/04_statistical_modeling.Rmd`**  
   Fits GLMMs and moderation models, producing model plots and diagnostics.
   
Outputs are written to:

`data/processed/` 

`figures/` 

---

## Authors

Milena Costa and Klāvs Kalvenieks

---

## Citation and Use of This Work

If you use, adapt, or build upon any part of this work, please cite:

> Costa, M., & Kalvenieks, K. *Statistical Consulting Project 13*. Leiden University.

This work is intended for academic and educational purposes.
