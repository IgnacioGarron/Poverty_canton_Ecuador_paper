create_output_dirs <- function(output_root) {
  dirs <- c(
    output_root,
    file.path(output_root, "folds"),
    file.path(output_root, "logs"),
    file.path(output_root, "metrics"),
    file.path(output_root, "models"),
    file.path(output_root, "predictions"),
    file.path(output_root, "tuning")
  )
  invisible(vapply(dirs, dir.create, logical(1), recursive = TRUE, showWarnings = FALSE))
}

check_pipeline_dependencies <- function(config) {
  required <- c(
    "tidymodels", "tune", "spatialsample", "sf", "glmnet", "xgboost",
    "mixOmics", "readxl", "readr", "haven", "corrplot"
  )
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop(
      "Faltan paquetes: ", paste(missing, collapse = ", "),
      ". Ejecute: Rscript 'setup_dependencies.R'"
    )
  }
  if ("xgboost_quantile" %in% config$models &&
      utils::packageVersion("xgboost") < "2.0.0") {
    stop("xgboost >= 2.0.0 es necesario para objective='reg:quantileerror'.")
  }
  invisible(TRUE)
}

attach_processing_packages <- function() {
  suppressPackageStartupMessages({
    library(tidymodels)
    library(spatialsample)
    library(sf)
    library(dplyr)
    library(readr)
    library(readxl)
    library(haven)
    library(ggplot2)
  })
  invisible(TRUE)
}

model_specification <- function(model_name, design, config) {
  switch(
    model_name,
    linear = make_linear_model(design, config),
    xgboost = make_xgboost_model(design, config),
    xgboost_quantile = make_xgboost_quantile_model(design, config),
    stop("Modelo desconocido: ", model_name)
  )
}

build_workflow <- function(training_data, predictor_names, mobile_predictor_names,
                           design, model_name, config) {
  recipe <- base_recipe(training_data, predictor_names) |>
    add_scenario_preprocessing(config$scenario_id, mobile_predictor_names) |>
    add_covariate_design(design, model_name)
  workflows::workflow() |>
    workflows::add_recipe(recipe) |>
    workflows::add_model(model_specification(model_name, design, config))
}

parameter_ranges <- function(workflow, training_data, predictor_names) {
  params <- hardhat::extract_parameter_set_dials(workflow)
  if (!nrow(params)) return(params)

  # Diez componentes evitan configuraciones imposibles en folds espaciales
  # pequeños o con covariables de varianza cero.
  max_components <- max(1L, min(10L, nrow(training_data) - 2L, length(predictor_names)))
  candidates <- list(
    mtry = dials::mtry_prop(c(0.20, 1.00)),
    trees = dials::trees(c(200L, 1200L)),
    min_n = dials::min_n(c(2L, 30L)),
    tree_depth = dials::tree_depth(c(2L, 10L)),
    learn_rate = dials::learn_rate(c(-3.0, -0.5)),
    loss_reduction = dials::loss_reduction(c(-6.0, 1.0)),
    sample_size = dials::sample_prop(c(0.50, 1.00)),
    xgb_alpha = dials::penalty(c(-6.0, 1.0)),
    xgb_lambda = dials::penalty(c(-6.0, 1.0)),
    penalty = dials::penalty(c(-5.0, 0.0)),
    num_comp = dials::num_comp(c(1L, max_components))
  )
  updates <- candidates[intersect(params$id, names(candidates))]
  params <- do.call(stats::update, c(list(object = params), updates))
  dials::finalize(params, training_data)
}

tuning_metrics <- function(model_name) {
  if (model_name == "xgboost_quantile") {
    metrics <- yardstick::metric_set(median_pinball_loss)
    # Compatibilidad con tune 2.1.0: tune_bayes() reconoce el modo cuantílico,
    # pero check_initial() vuelve a validar el set y omite quantile_metric_set.
    # La métrica sigue siendo pinball; la clase adicional solo supera ese segundo
    # chequeo y puede retirarse cuando tune lo corrija upstream.
    class(metrics) <- unique(c("numeric_metric_set", class(metrics)))
    return(metrics)
  }
  yardstick::metric_set(yardstick::rmse, yardstick::mae)
}

tuning_metric_name <- function(model_name) {
  if (model_name == "xgboost_quantile") "median_pinball_loss" else "rmse"
}

make_spatial_folds <- function(data_sf, v, config, seed_offset = 0L) {
  if (!inherits(data_sf, "sf")) stop("La validación espacial requiere un objeto sf.")
  if (v >= nrow(data_sf)) stop("v debe ser menor que el número de cantones.")
  set.seed(config$seed + seed_offset)
  spatialsample::spatial_clustering_cv(
    data_sf,
    v = v,
    cluster_function = config$cluster_function
  )
}

fold_membership <- function(folds) {
  purrr::map2_dfr(folds$splits, folds$id, function(split, fold_id) {
    spatialsample::assessment(split) |>
      sf::st_drop_geometry() |>
      dplyr::transmute(CodCanton = .data$CodCanton, outer_fold = fold_id)
  })
}

tune_workflow_bayes <- function(workflow, resamples, training_data,
                                predictor_names, model_name, config,
                                seed_offset = 0L) {
  params <- parameter_ranges(workflow, training_data, predictor_names)
  n_params <- nrow(params)
  if (!n_params) stop("La combinación no contiene parámetros ajustables.")
  initial <- max(config$bayes_initial, n_params + 1L)

  set.seed(config$seed + seed_offset)
  result <- tune::tune_bayes(
    workflow,
    resamples = resamples,
    initial = initial,
    iter = config$bayes_iter,
    param_info = params,
    metrics = tuning_metrics(model_name),
    control = tune::control_bayes(
      verbose = TRUE,
      no_improve = config$bayes_no_improve,
      save_pred = FALSE,
      save_workflow = TRUE
    )
  )
  best <- tune::select_best(result, metric = tuning_metric_name(model_name))
  list(result = result, best = best)
}

extract_numeric_prediction <- function(fitted_workflow, new_data, model_name) {
  if (model_name != "xgboost_quantile") {
    return(as.numeric(predict(fitted_workflow, new_data = new_data)$.pred))
  }
  pred <- predict(fitted_workflow, new_data = new_data, type = "quantile")$.pred_quantile
  quantile_levels <- attr(pred, "quantile_levels")
  position <- which.min(abs(quantile_levels - 0.5))
  as.numeric(as.matrix(pred)[, position])
}

write_tuning_outputs <- function(tuned, combo_id, fold_id, output_dir, save_object) {
  metrics_path <- file.path(
    output_dir, "tuning", paste0("history__", combo_id, "__", fold_id, ".csv")
  )
  readr::write_csv(tune::collect_metrics(tuned$result), metrics_path)
  best <- dplyr::mutate(tuned$best, fold = fold_id, .before = 1L)
  readr::write_csv(
    best,
    file.path(output_dir, "tuning", paste0("best__", combo_id, "__", fold_id, ".csv"))
  )
  if (save_object) {
    saveRDS(
      tuned$result,
      file.path(output_dir, "tuning", paste0("tune_object__", combo_id, "__", fold_id, ".rds"))
    )
  }
  best
}

run_nested_spatial_combo <- function(spatial_data, frames, outer_folds,
                                     design, model_name, config) {
  combo_id <- paste(model_name, design, sep = "__")
  message("\n=== ", config$scenario_id, " / ", combo_id, " ===")
  oof_parts <- vector("list", nrow(outer_folds))
  outer_best <- vector("list", nrow(outer_folds))

  for (i in seq_len(nrow(outer_folds))) {
    split <- outer_folds$splits[[i]]
    analysis_sf <- spatialsample::analysis(split)
    assessment_sf <- spatialsample::assessment(split)
    analysis_df <- sf::st_drop_geometry(analysis_sf)
    assessment_df <- sf::st_drop_geometry(assessment_sf)
    inner_folds <- make_spatial_folds(
      analysis_sf, config$inner_v, config,
      seed_offset = 1000L * i + match(design, config$designs) * 10L + match(model_name, config$models)
    )
    workflow <- build_workflow(
      analysis_df, frames$predictor_names, frames$mobile_predictor_names,
      design, model_name, config
    )
    tuned <- tune_workflow_bayes(
      workflow, inner_folds, analysis_df, frames$predictor_names,
      model_name, config,
      seed_offset = 10000L + i * 100L
    )
    fold_id <- as.character(outer_folds$id[[i]])
    outer_best[[i]] <- write_tuning_outputs(
      tuned, combo_id, fold_id, config$output_root, config$save_tune_objects
    )
    final_workflow <- tune::finalize_workflow(workflow, tuned$best)
    fitted <- parsnip::fit(final_workflow, data = analysis_df)
    estimate <- extract_numeric_prediction(fitted, assessment_df, model_name)
    oof_parts[[i]] <- tibble::tibble(
      CodCanton = assessment_df$CodCanton,
      Canton = assessment_df$Canton,
      fold = fold_id,
      truth = assessment_df$Y,
      estimate = estimate
    )
  }

  oof <- dplyr::bind_rows(oof_parts) |>
    dplyr::arrange(.data$CodCanton)
  if (nrow(oof) != nrow(spatial_data) || anyDuplicated(oof$CodCanton)) {
    stop("Las predicciones OOF no contienen exactamente un registro por cantón.")
  }
  readr::write_csv(
    oof,
    file.path(config$output_root, "predictions", paste0("oof__", combo_id, ".csv"))
  )

  oof_metrics <- overall_metrics(oof, "nested_spatial_oof")
  group_metrics <- group_error_metrics(oof)
  readr::write_csv(
    group_metrics,
    file.path(config$output_root, "metrics", paste0("group_oof__", combo_id, ".csv"))
  )

  # Ajuste final sobre la misma muestra continental usada en la evaluación.
  # Los tres cantones de Galápagos se excluyen antes de construir los frames.
  spatial_df <- sf::st_drop_geometry(spatial_data)
  full_df <- frames$df_2022
  full_workflow <- build_workflow(
    full_df, frames$predictor_names, frames$mobile_predictor_names,
    design, model_name, config
  )
  final_tuned <- tune_workflow_bayes(
    full_workflow, outer_folds, full_df, frames$predictor_names,
    model_name, config, seed_offset = 900000L
  )
  final_best <- write_tuning_outputs(
    final_tuned, combo_id, "final", config$output_root, config$save_tune_objects
  )
  final_workflow <- tune::finalize_workflow(full_workflow, final_tuned$best)
  final_fit <- parsnip::fit(final_workflow, data = full_df)
  saveRDS(
    final_fit,
    file.path(config$output_root, "models", paste0(combo_id, ".rds"))
  )

  in_sample <- tibble::tibble(
    CodCanton = spatial_df$CodCanton,
    Canton = spatial_df$Canton,
    truth = spatial_df$Y,
    estimate = extract_numeric_prediction(final_fit, spatial_df, model_name)
  )
  readr::write_csv(
    in_sample,
    file.path(config$output_root, "predictions", paste0("in_sample__", combo_id, ".csv"))
  )
  in_sample_metrics <- overall_metrics(in_sample, "in_sample_diagnostic")
  gap <- generalization_gap(oof_metrics, in_sample_metrics)

  prediction_2023 <- frames$df_2023 |>
    dplyr::transmute(
      CodCanton = .data$CodCanton,
      Canton = .data$Canton,
      prediction_2023 = extract_numeric_prediction(final_fit, frames$df_2023, model_name),
      reference_mef_2023 = frames$y_2023_mef,
      reference_enemdu_2023 = frames$y_2023_enemdu
    )
  readr::write_csv(
    prediction_2023,
    file.path(config$output_root, "predictions", paste0("prediction_2023__", combo_id, ".csv"))
  )

  overall <- dplyr::bind_rows(oof_metrics, in_sample_metrics) |>
    dplyr::mutate(scenario = config$scenario_id, model = model_name, design = design, .before = 1L)
  gap <- gap |>
    dplyr::mutate(scenario = config$scenario_id, model = model_name, design = design, .before = 1L)
  readr::write_csv(
    overall,
    file.path(config$output_root, "metrics", paste0("overall__", combo_id, ".csv"))
  )
  readr::write_csv(
    gap,
    file.path(config$output_root, "metrics", paste0("generalization_gap__", combo_id, ".csv"))
  )

  list(overall = overall, gap = gap, groups = group_metrics, final_best = final_best)
}

combination_grid <- function(config) {
  tidyr::crossing(model = config$models, design = config$designs) |>
    dplyr::mutate(
      status = dplyr::if_else(
        .data$model == "linear" & .data$design == "full",
        "not_estimable",
        "pending"
      ),
      reason = dplyr::if_else(
        .data$status == "not_estimable",
        "OLS full no tiene hiperparámetros para tune_bayes y p >> n; no formaba parte del modelo lineal original.",
        NA_character_
      )
    )
}

run_scenario <- function(repo_root, scenario_id) {
  config <- pipeline_config(repo_root, scenario_id)
  validate_config(config)
  create_output_dirs(config$output_root)
  check_pipeline_dependencies(config)
  attach_processing_packages()
  saveRDS(config, file.path(config$output_root, "run_config.rds"))
  writeLines(
    utils::capture.output(utils::sessionInfo()),
    file.path(config$output_root, "session_info.txt")
  )

  spec <- scenario_spec(scenario_id)
  message("Procesando escenario: ", scenario_id)
  processed <- load_processing_output(config, spec)
  frames <- build_model_frames(processed, config$output_root)
  spatial_data <- join_canton_geometry(frames$df_2022, repo_root, config$output_root)
  message(
    "Validación espacial: ", nrow(spatial_data), " de ", nrow(frames$df_2022),
    " cantones tienen geometría."
  )

  outer_folds <- make_spatial_folds(spatial_data, config$outer_v, config)
  membership <- fold_membership(outer_folds)
  readr::write_csv(
    membership,
    file.path(config$output_root, "folds", "outer_spatial_fold_membership.csv")
  )
  saveRDS(outer_folds, file.path(config$output_root, "folds", "outer_spatial_folds.rds"))

  grid <- combination_grid(config)
  readr::write_csv(grid, file.path(config$output_root, "combination_manifest.csv"))
  results <- list()

  for (i in seq_len(nrow(grid))) {
    if (grid$status[[i]] == "not_estimable") next
    model_name <- grid$model[[i]]
    design <- grid$design[[i]]
    combo_id <- paste(model_name, design, sep = "__")

    attempt <- tryCatch(
      run_nested_spatial_combo(
        spatial_data, frames, outer_folds, design, model_name, config
      ),
      error = function(e) e
    )
    if (inherits(attempt, "error")) {
      grid$status[[i]] <- "failed"
      grid$reason[[i]] <- conditionMessage(attempt)
      writeLines(
        c(conditionMessage(attempt), utils::capture.output(traceback())),
        file.path(config$output_root, "logs", paste0("error__", combo_id, ".txt"))
      )
      if (!config$continue_on_error) stop(attempt)
    } else {
      grid$status[[i]] <- "completed"
      results[[combo_id]] <- attempt
    }
    readr::write_csv(grid, file.path(config$output_root, "combination_manifest.csv"))
  }

  if (length(results)) {
    readr::write_csv(
      dplyr::bind_rows(purrr::map(results, "overall")),
      file.path(config$output_root, "metrics", "all_overall_metrics.csv")
    )
    readr::write_csv(
      dplyr::bind_rows(purrr::map(results, "gap")),
      file.path(config$output_root, "metrics", "all_generalization_gaps.csv")
    )
    readr::write_csv(
      dplyr::bind_rows(purrr::imap(results, function(x, id) {
        tidyr::separate_wider_delim(
          dplyr::mutate(x$groups, combo = id, .before = 1L),
          .data$combo, delim = "__", names = c("model", "design")
        ) |>
          dplyr::mutate(scenario = config$scenario_id, .before = 1L)
      })),
      file.path(config$output_root, "metrics", "all_group_oof_errors.csv")
    )
  }

  readr::write_csv(grid, file.path(config$output_root, "combination_manifest.csv"))
  invisible(list(config = config, manifest = grid, results = results))
}
