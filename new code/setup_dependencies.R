cran_packages <- c(
  "tidymodels", "spatialsample", "sf", "glmnet", "xgboost", "readxl",
  "readr", "haven", "corrplot", "BiocManager"
)
missing_cran <- cran_packages[!vapply(cran_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_cran)) {
  install.packages(missing_cran, repos = "https://cloud.r-project.org")
}
if (utils::packageVersion("xgboost") < "2.0.0") {
  install.packages("xgboost", repos = "https://cloud.r-project.org")
}
if (!requireNamespace("mixOmics", quietly = TRUE)) {
  BiocManager::install("mixOmics", ask = FALSE, update = FALSE)
}

message("Dependencias instaladas. Reinicie R antes de ejecutar los escenarios.")

