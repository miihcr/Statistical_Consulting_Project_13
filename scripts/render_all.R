# scripts/render_all.R

render_one <- function(file) {
  message("\n--- Rendering: ", file, " ---")
  rmarkdown::render(input = file, envir = new.env(parent = globalenv()))
  message("✓ Done: ", file)
}

# your Rmds are in code/
r1 <- here::here("code","1_data_preparation.Rmd")
r2 <- here::here("code","2_descriptives.Rmd")
r3 <- here::here("code","3_analysis.Rmd")

render_one(r1)
render_one(r2)
render_one(r3)

message("\nAll reports rendered.")
