source_new_pipeline <- function(repo_root) {
  root <- file.path(repo_root, "new code")
  files <- c(
    "R/config.R",
    "R/metrics.R",
    "R/data_helpers.R",
    "R/step_lasso_select.R",
    "R/covariate_designs.R",
    "models/model_linear.R",
    "models/model_xgboost.R",
    "models/model_xgboost_quantile.R",
    "R/pipeline.R"
  )
  for (file in files) sys.source(file.path(root, file), envir = globalenv())
  invisible(TRUE)
}

