# ---- packages ----
pkgs <- c(
  "dplyr", "tidyr", "stringr", "ggplot2", "here", "readr",
  "ggthemes", "glmmTMB",
  "lme4", "lmerTest", "emmeans", "car", "ggeffects", "DHARMa", "performance",
  "igraph", "lavaan", "purrr", "psych", "knitr",
  "mice", "VIM", "naniar", "gtsummary"
)

# Install package if not there
for (pkg in pkgs) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    stop(paste0("Package '", pkg, "' is not installed. Please install it with: install.packages('", pkg, "')"))
  }
}

# ---- knitr & ggplot defaults (safe to call from Rmds) ----
if (requireNamespace("knitr", quietly = TRUE)) {
  knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
}
theme_set(ggplot2::theme_minimal())

# ---- paths & data loader ----
dir.create(here::here("data","processed"), recursive = TRUE, showWarnings = FALSE)

load_processed <- function() {
  fw <- here::here("data","processed","df_wide.rds")
  fl <- here::here("data","processed","df_long.rds")
  list(
    df_wide = if (file.exists(fw)) readRDS(fw) else NULL,
    df_long = if (file.exists(fl)) readRDS(fl) else NULL
  )
}

# Scripts are called when needed
load_processed_data <- function(assign_global = FALSE) {
  fw <- here::here("data","processed","df_wide.rds")
  fl <- here::here("data","processed","df_long.rds")
  
  data_list <- list(
    df_wide = if (file.exists(fw)) readRDS(fw) else NULL,
    df_long = if (file.exists(fl)) readRDS(fl) else NULL
  )
  
  if (assign_global) {
    list2env(data_list, envir = .GlobalEnv)
  }
  
  invisible(data_list)
}


