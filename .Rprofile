# .Rprofile — Project startup file

common_path <- file.path(getwd(), "scripts", "_common.R")

if (file.exists(common_path)) {
  tryCatch(
    {
      source(common_path)
      message("✔ Packages and utilities loaded from scripts/_common.R")
    },
    error = function(e) {
      message("⚠ Could not load scripts/_common.R: ", e$message)
      message("   (Tip: install missing packages, then restart R)")
    }
  )
} else {
  message("⚠ scripts/_common.R not found. Are you in the project root?")
}
