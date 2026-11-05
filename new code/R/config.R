env_int <- function(name, default) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) return(as.integer(default))
  out <- suppressWarnings(as.integer(value))
  if (is.na(out)) stop(name, " debe ser un entero; se recibió: ", value)
  out
}

env_flag <- function(name, default = FALSE) {
  value <- tolower(Sys.getenv(name, unset = if (default) "true" else "false"))
  if (!value %in% c("true", "false", "1", "0", "yes", "no")) {
    stop(name, " debe ser TRUE/FALSE; se recibió: ", value)
  }
  value %in% c("true", "1", "yes")
}

env_vector <- function(name, default) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) return(default)
  trimws(strsplit(value, ",", fixed = TRUE)[[1]])
}

pipeline_config <- function(repo_root, scenario_id) {
  spatial_v <- env_int("SPATIAL_V", 5L)
  list(
    repo_root = normalizePath(repo_root, mustWork = TRUE),
    scenario_id = scenario_id,
    output_root = file.path(repo_root, "output_tidymodels", scenario_id),
    seed = env_int("SEED", 123L),
    outer_v = env_int("SPATIAL_OUTER_V", spatial_v),
    inner_v = env_int("SPATIAL_INNER_V", spatial_v),
    cluster_function = Sys.getenv("SPATIAL_CLUSTER_FUNCTION", unset = "kmeans"),
    bayes_initial = env_int("BAYES_INITIAL", 15L),
    bayes_iter = env_int("BAYES_ITER", 25L),
    bayes_no_improve = env_int("BAYES_NO_IMPROVE", 10L),
    xgb_threads = env_int("XGB_THREADS", 1L),
    designs = env_vector(
      "DESIGNS",
      c("full", "pcr", "pls", "lasso", "pcr_lasso")
    ),
    models = env_vector(
      "MODELS",
      c("linear", "xgboost", "xgboost_quantile")
    ),
    save_tune_objects = env_flag("SAVE_TUNE_OBJECTS", FALSE),
    continue_on_error = env_flag("CONTINUE_ON_ERROR", TRUE)
  )
}

validate_config <- function(config) {
  valid_designs <- c("full", "pcr", "pls", "lasso", "pcr_lasso")
  valid_models <- c("linear", "xgboost", "xgboost_quantile")
  if (!all(config$designs %in% valid_designs)) {
    stop("DESIGNS contiene valores no válidos: ",
         paste(setdiff(config$designs, valid_designs), collapse = ", "))
  }
  if (!all(config$models %in% valid_models)) {
    stop("MODELS contiene valores no válidos: ",
         paste(setdiff(config$models, valid_models), collapse = ", "))
  }
  if (!config$cluster_function %in% c("kmeans", "hclust")) {
    stop("SPATIAL_CLUSTER_FUNCTION debe ser 'kmeans' o 'hclust'.")
  }
  if (config$outer_v < 2L || config$inner_v < 2L) {
    stop("Los números de folds espaciales deben ser al menos 2.")
  }
  invisible(config)
}

