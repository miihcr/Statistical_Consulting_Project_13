source(here::here("scripts", "_common.R"))
source_R()

message("Running 01_compute_moderators...")
source(here::here("scripts", "01_compute_moderators.R"))

message("Running 02_prepare_data...")
source(here::here("scripts", "02_prepare_data.R"))

message("Running 03_descriptives...")
source(here::here("scripts", "03_descriptives.R"))

message("Running 04_missingness...")
source(here::here("scripts", "04_missingness.R"))

message("Running 05_modeling...")
source(here::here("scripts", "05_modeling.R"))

message("Pipeline completed successfully.")
