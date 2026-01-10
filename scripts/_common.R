# _common.R
# Shared utilities and package loading for Statistical Consulting Project 13

pkgs <- c(
  "dplyr", "tidyr", "stringr", "ggplot2", "here", "readr",
  "ggthemes", "glmmTMB", "effectsize", "sjPlot", "semPlot",
  "lme4", "lmerTest", "emmeans", "ggeffects", "DHARMa",
  "performance", "igraph", "lavaan", "purrr", "psych", "knitr",
  "mice", "VIM", "naniar", "gtsummary", "tidyselect"
)

for (pkg in pkgs) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    stop(paste0("Package '", pkg, "' is not installed."))
  }
}

# Resolve masking issues

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

# ---- Project paths + output folders ----
p_fig <- function(...) here::here("figures", ...)
p_processed <- function(...) here::here("data", "processed", ...)

# Ensure common output folders exist
dir.create(p_fig(), recursive = TRUE, showWarnings = FALSE)
dir.create(p_processed(), recursive = TRUE, showWarnings = FALSE)

# folders for saved outputs
dir.create(p_processed("results"), recursive = TRUE, showWarnings = FALSE)

# ---- Processed data loader ----
load_processed <- function() {
  fw <- p_processed("df_wide.rds")
  fl <- p_processed("df_long.rds")
  
  if (!file.exists(fw)) stop("Missing processed file: ", fw)
  if (!file.exists(fl)) stop("Missing processed file: ", fl)
  
  list(
    df_wide = readRDS(fw),
    df_long = readRDS(fl)
  )
}

load_processed_data <- function(assign_global = FALSE) {
  data_list <- load_processed()
  if (assign_global) list2env(data_list, envir = .GlobalEnv)
  invisible(data_list)
}


