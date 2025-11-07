# ---- packages ----
pkgs <- c(
  "dplyr","tidyr","stringr","ggplot2","here","readr",
  "ggthemes",
  "lme4","lmerTest","emmeans","car","ggeffects","DHARMa","performance",
  "igraph","lavaan","purrr","psych","knitr"
)
invisible(lapply(pkgs, library, character.only = TRUE))

# ---- knitr & ggplot defaults (safe to call from Rmds) ----
if (requireNamespace("knitr", quietly = TRUE)) {
  knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)
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

.loaded <- load_processed()
df_wide <- .loaded$df_wide
df_long <- .loaded$df_long
rm(.loaded)
