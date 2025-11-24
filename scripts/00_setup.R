# 00_setup.R

# Pipeline for Statistical Consulting Project 13

source("scripts/_common.R")

source("scripts/01_compute_moderators.R")
source("scripts/02_prepare_data.R")
source("scripts/03_descriptives.R")
source("scripts/04_missingness.R")
source("scripts/05_modeling.R")

message("Pipeline completed successfully.")
