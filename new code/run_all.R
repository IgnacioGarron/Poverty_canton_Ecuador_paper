new_code_root <- normalizePath(getwd(), mustWork = TRUE)
repo_root <- normalizePath(file.path(new_code_root, ".."), mustWork = TRUE)
source(file.path(new_code_root, "R", "load_all.R"))
source_new_pipeline(repo_root)

available <- scenario_catalog()$scenario_id
requested_text <- Sys.getenv("SCENARIOS", unset = "")
requested <- if (nzchar(requested_text)) {
  trimws(strsplit(requested_text, ",", fixed = TRUE)[[1]])
} else {
  available
}
if (!all(requested %in% available)) {
  stop("SCENARIOS contiene escenarios desconocidos: ",
       paste(setdiff(requested, available), collapse = ", "))
}

for (scenario_id in requested) run_scenario(repo_root, scenario_id)
