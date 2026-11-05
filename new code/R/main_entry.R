scenario_main <- function(scenario_id) {
  new_code_root <- normalizePath(getwd(), mustWork = TRUE)
  repo_root <- normalizePath(file.path(new_code_root, ".."), mustWork = TRUE)
  sys.source(file.path(new_code_root, "R", "load_all.R"), envir = globalenv())
  source_new_pipeline(repo_root)
  run_scenario(repo_root, scenario_id)
}
