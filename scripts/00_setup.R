# 00_setup.R - Setup


# ---- Packages ----
pkgs <- c(
  "dplyr", "tidyr", "stringr", "ggplot2", "here", "readr",
  "ggthemes", "lme4", "lmerTest", "emmeans", "car",
  "ggeffects", "DHARMa", "performance", "igraph", "lavaan",
  "purrr", "psych", "knitr", "mice", "VIM", "naniar", "gtsummary"
)

for (pkg in pkgs) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    stop(paste0("Package '", pkg, "' is not installed. Install with: install.packages('", pkg, "')"))
  }
}

# ---- knitr options ----
if (requireNamespace("knitr", quietly = TRUE)) {
  knitr::opts_chunk$set(
    echo = FALSE, message = FALSE, warning = FALSE,
    fig.path = here::here("analysis", "figures", "")
  )
}

# ---- ggplot theme ----
theme_set(ggplot2::theme_minimal())

# ---- Ensure directories ----
dir.create(here::here("data", "processed"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("analysis", "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("report", "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("report", "figures"), recursive = TRUE, showWarnings = FALSE)

# ---- Helper functions ----

load_processed <- function() {
  fw <- here::here("data", "processed", "df_wide.rds")
  fl <- here::here("data", "processed", "df_long.rds")
  list(
    df_wide = if (file.exists(fw)) readRDS(fw) else NULL,
    df_long = if (file.exists(fl)) readRDS(fl) else NULL
  )
}

save_processed <- function(df_wide, df_long) {
  saveRDS(df_wide, here::here("data", "processed", "df_wide.rds"))
  saveRDS(df_long, here::here("data", "processed", "df_long.rds"))
}