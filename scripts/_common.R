# _common.R
# Shared utilities and package loading for Statistical Consulting Project 13

pkgs <- c(
  "dplyr", "tidyr", "stringr", "ggplot2", "here", "readr",
  "ggthemes", "glmmTMB", "effectsize", "sjPlot", "semPlot",
  "lme4", "lmerTest", "emmeans","ggeffects", "DHARMa",
  "performance", "igraph", "lavaan", "purrr", "psych", "knitr",
  "mice", "VIM", "naniar", "gtsummary"
)

for (pkg in pkgs) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    stop(paste0("Package '", pkg, "' is not installed."))
  }
}

# Resolve masking issues
library(dplyr)
library(tidyselect)

select      <- dplyr::select
filter      <- dplyr::filter
mutate      <- dplyr::mutate
rename      <- dplyr::rename
arrange     <- dplyr::arrange

matches     <- tidyselect::matches
starts_with <- tidyselect::starts_with
contains    <- tidyselect::contains

if (requireNamespace("knitr", quietly = TRUE)) {
  knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
}

theme_set(ggplot2::theme_minimal())

# Ensure processed folder exists
dir.create(here::here("data", "processed"), recursive = TRUE, showWarnings = FALSE)

load_processed <- function() {
  fw <- here::here("data", "processed", "df_wide.rds")
  fl <- here::here("data", "processed", "df_long.rds")
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
