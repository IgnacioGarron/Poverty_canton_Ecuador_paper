new_code_root <- normalizePath(getwd(), mustWork = TRUE)
repo_root <- normalizePath(file.path(new_code_root, ".."), mustWork = TRUE)
source(file.path(new_code_root, "R", "load_all.R"))
source_new_pipeline(repo_root)

config <- pipeline_config(repo_root, "baseline")
config$output_root <- file.path(tempdir(), "poverty_tidymodels_smoke")
create_output_dirs(config$output_root)
check_pipeline_dependencies(config)
attach_processing_packages()

processed <- load_processing_output(config, scenario_spec("baseline"))
frames <- build_model_frames(processed, config$output_root)
spatial_data <- join_canton_geometry(frames$df_2022, repo_root, config$output_root)
stopifnot(
  nrow(frames$df_2022) == 218L,
  nrow(frames$df_2023) == 218L,
  nrow(spatial_data) == 218L,
  !any(frames$df_2022$CodCanton %in% excluded_analysis_canton_codes()),
  !any(spatial_data$CodCanton %in% excluded_analysis_canton_codes())
)

training <- sf::st_drop_geometry(spatial_data)
for (design in config$designs) {
  for (model_name in config$models) {
    if (design == "full" && model_name == "linear") next
    workflow <- build_workflow(
      training, frames$predictor_names, frames$mobile_predictor_names,
      design, model_name, config
    )
    params <- parameter_ranges(workflow, training, frames$predictor_names)
    stopifnot(nrow(params) > 0L)
  }
}

message("Smoke test superado: datos, geometría, 14 workflows y rangos de tuning.")
