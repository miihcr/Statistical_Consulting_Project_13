# ---- packages ----
pkgs <- c(
  "dplyr", "tidyr", "stringr", "ggplot2", "here", "readr",
  "ggthemes", "glmmTMB", "effectsize", "sjPlot", "semPlot",
  "lme4", "lmerTest", "emmeans", "car", "ggeffects", "DHARMa", "performance",
  "igraph", "lavaan", "purrr", "psych", "knitr",
  "mice", "VIM", "naniar", "gtsummary"
)

# Load packages (stop if missing)
for (pkg in pkgs) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    stop(paste0("Package '", pkg, "' is not installed. Install it with: install.packages('", pkg, "')"))
  }
}

# ---- fix masking (MASS, car, psych) ----
# Re-load dplyr last so its functions override others
library(dplyr)
library(tidyselect)

# force dplyr / tidyselect versions for safety
select      <- dplyr::select
filter      <- dplyr::filter
mutate      <- dplyr::mutate
rename      <- dplyr::rename
arrange     <- dplyr::arrange

matches     <- tidyselect::matches
starts_with <- tidyselect::starts_with
ends_with   <- tidyselect::ends_with
contains    <- tidyselect::contains

# ---- knitr & ggplot ----
if (requireNamespace("knitr", quietly = TRUE)) {
  knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
}

theme_set(ggplot2::theme_minimal())

# ---- paths & data loading ----
dir.create(here::here("data","processed"), recursive = TRUE, showWarnings = FALSE)

load_processed <- function() {
  fw <- here::here("data","processed","df_wide.rds")
  fl <- here::here("data","processed","df_long.rds")
  list(
    df_wide = if (file.exists(fw)) readRDS(fw) else NULL,
    df_long = if (file.exists(fl)) readRDS(fl) else NULL
  )
}

load_processed_data <- function(assign_global = FALSE) {
  data_list <- load_processed()
  if (assign_global) list2env(data_list, envir = .GlobalEnv)
  invisible(data_list)
}
