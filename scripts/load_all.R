# scripts/load_all.R
source(here::here("R","_common.R"), local = globalenv())

proc <- here::here("data","processed")
stopifnot(
  file.exists(file.path(proc,"df_wide.rds")),
  file.exists(file.path(proc,"df_long.rds"))
)

df_wide <- readRDS(file.path(proc,"df_wide.rds"))
df_long <- readRDS(file.path(proc,"df_long.rds"))
message("Loaded df_wide: ", nrow(df_wide),
        " | df_long: ", nrow(df_long))
