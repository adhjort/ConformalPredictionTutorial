# --- setup.R ----------------------------------------------------------

# Load or install required packages
required_pkgs <- c(
  "tidyverse",
  "ggplot2",
  "dplyr",
  "data.table",
  "matrixStats",
  "tidyverse", 
  "xgboost", 
  "nnet", 
  "tidymodels"
)

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

message("Setup complete ✓")


