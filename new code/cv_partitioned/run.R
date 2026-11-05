#!/usr/bin/env Rscript

###############################################################
## Repeated nested spatial CV for the incremental value of
## mobile mobility data in the Full XGBoost specification.
##
## Comparisons:
##   1. Administrative covariates versus administrative + mobility PC1
##   2. Administrative covariates versus administrative + 38 mobility inputs
###############################################################

find_project_root <- function(start_directory = getwd()) {
  current_directory <- normalizePath(start_directory, winslash = "/", mustWork = TRUE)
  repeat {
    if (
      file.exists(file.path(current_directory, "new code", "R", "load_all.R")) &&
        dir.exists(file.path(current_directory, "output_tidymodels"))
    ) {
      return(current_directory)
    }
    parent_directory <- dirname(current_directory)
    if (identical(parent_directory, current_directory)) {
      stop("Could not locate the poverty_pred_Ecuador project root.")
    }
    current_directory <- parent_directory
  }
}

env_double <- function(name, default) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) return(as.numeric(default))
  output <- suppressWarnings(as.numeric(value))
  if (!is.finite(output)) stop(name, " must be numeric; received: ", value)
  output
}

format_number <- function(x, digits = 2L) {
  formatC(x, format = "f", digits = digits)
}

latex_escape <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([#$%&_{}])", "\\\\\\1", x, perl = TRUE)
  x <- gsub("~", "\\\\textasciitilde{}", x, fixed = TRUE)
  x <- gsub("\\^", "\\\\textasciicircum{}", x)
  x
}

prepare_scenario <- function(repo_root, scenario_id, prepared_root) {
  scenario_output <- file.path(prepared_root, scenario_id)
  dir.create(scenario_output, recursive = TRUE, showWarnings = FALSE)

  config <- pipeline_config(repo_root, scenario_id)
  config$output_root <- scenario_output
  config$designs <- "full"
  config$models <- "xgboost"
  config$continue_on_error <- FALSE

  spec <- scenario_spec(scenario_id)
  processed <- load_processing_output(config, spec)
  frames <- build_model_frames(processed, scenario_output)
  spatial_data <- join_canton_geometry(frames$df_2022, repo_root, scenario_output) |>
    dplyr::arrange(.data$CodCanton)

  list(config = config, frames = frames, spatial_data = spatial_data)
}

validate_scenario_alignment <- function(scenario_inputs, scenarios) {
  reference <- scenario_inputs[["baseline"]]$spatial_data |>
    sf::st_drop_geometry() |>
    dplyr::arrange(.data$CodCanton) |>
    dplyr::select("CodCanton", "Canton", "Y")

  for (scenario_id in setdiff(scenarios, "baseline")) {
    candidate <- scenario_inputs[[scenario_id]]$spatial_data |>
      sf::st_drop_geometry() |>
      dplyr::arrange(.data$CodCanton) |>
      dplyr::select("CodCanton", "Canton", "Y")

    if (!identical(reference$CodCanton, candidate$CodCanton)) {
      stop("The canton samples are not aligned for scenario: ", scenario_id)
    }
    if (!isTRUE(all.equal(reference$Y, candidate$Y, tolerance = 1e-12))) {
      stop("The poverty outcomes are not aligned for scenario: ", scenario_id)
    }
  }

  if (nrow(reference) != 218L) {
    stop("Expected 218 continental cantons; found ", nrow(reference), ".")
  }
  invisible(TRUE)
}

validate_workflows <- function(scenario_inputs, scenarios) {
  for (scenario_id in scenarios) {
    scenario_input <- scenario_inputs[[scenario_id]]
    training_data <- sf::st_drop_geometry(scenario_input$spatial_data)
    workflow <- build_workflow(
      training_data,
      scenario_input$frames$predictor_names,
      scenario_input$frames$mobile_predictor_names,
      design = "full",
      model_name = "xgboost",
      config = scenario_input$config
    )
    parameters <- hardhat::extract_parameter_set_dials(workflow)
    message(
      "Validated ", scenario_id, ": ",
      length(scenario_input$frames$predictor_names), " raw predictors and ",
      nrow(parameters), " tunable parameters."
    )
  }
  invisible(TRUE)
}

make_repeat_membership <- function(reference_spatial_data, config, repeat_id, repeat_seed) {
  config$seed <- repeat_seed
  folds <- make_spatial_folds(reference_spatial_data, config$outer_v, config)
  membership <- fold_membership(folds) |>
    dplyr::mutate(
      repetition = repeat_id,
      seed = repeat_seed,
      .before = 1L
    )

  if (
    nrow(membership) != nrow(reference_spatial_data) ||
      anyDuplicated(membership$CodCanton)
  ) {
    stop("Repeat ", repeat_id, " does not assign each canton to exactly one outer fold.")
  }
  membership
}

run_repeated_oof <- function(spatial_data, frames, membership, config,
                             repeat_id, repeat_seed, scenario_id) {
  combo_id <- "xgboost__full"
  config$scenario_id <- scenario_id
  config$seed <- repeat_seed
  config$designs <- "full"
  config$models <- "xgboost"
  config$continue_on_error <- FALSE
  create_output_dirs(config$output_root)

  saveRDS(config, file.path(config$output_root, "run_config.rds"))
  fold_ids <- membership |>
    dplyr::distinct(.data$outer_fold) |>
    dplyr::pull(.data$outer_fold)

  expected_codes <- sort(as.character(spatial_data$CodCanton))
  membership_codes <- sort(as.character(membership$CodCanton))
  if (!identical(expected_codes, membership_codes)) {
    stop("Outer-fold membership does not match scenario ", scenario_id, ".")
  }

  oof_parts <- vector("list", length(fold_ids))
  best_parts <- vector("list", length(fold_ids))

  for (i in seq_along(fold_ids)) {
    fold_id <- fold_ids[[i]]
    assessment_codes <- membership |>
      dplyr::filter(.data$outer_fold == fold_id) |>
      dplyr::pull(.data$CodCanton)

    analysis_sf <- spatial_data |>
      dplyr::filter(!(.data$CodCanton %in% assessment_codes))
    assessment_sf <- spatial_data |>
      dplyr::filter(.data$CodCanton %in% assessment_codes)
    analysis_df <- sf::st_drop_geometry(analysis_sf)
    assessment_df <- sf::st_drop_geometry(assessment_sf)

    inner_folds <- make_spatial_folds(
      analysis_sf,
      config$inner_v,
      config,
      seed_offset = 1000L * i + 11L
    )
    workflow <- build_workflow(
      analysis_df,
      frames$predictor_names,
      frames$mobile_predictor_names,
      design = "full",
      model_name = "xgboost",
      config = config
    )
    tuned <- tune_workflow_bayes(
      workflow,
      inner_folds,
      analysis_df,
      frames$predictor_names,
      model_name = "xgboost",
      config = config,
      seed_offset = 10000L + i * 100L
    )
    best_parts[[i]] <- write_tuning_outputs(
      tuned,
      combo_id,
      fold_id,
      config$output_root,
      config$save_tune_objects
    )

    final_workflow <- tune::finalize_workflow(workflow, tuned$best)
    fitted_workflow <- parsnip::fit(final_workflow, data = analysis_df)
    estimate <- extract_numeric_prediction(
      fitted_workflow,
      new_data = assessment_df,
      model_name = "xgboost"
    )

    oof_parts[[i]] <- tibble::tibble(
      repetition = repeat_id,
      seed = repeat_seed,
      scenario = scenario_id,
      CodCanton = assessment_df$CodCanton,
      Canton = assessment_df$Canton,
      fold = fold_id,
      truth = assessment_df$Y,
      estimate = estimate
    )
  }

  predictions <- dplyr::bind_rows(oof_parts) |>
    dplyr::arrange(.data$CodCanton)
  if (nrow(predictions) != 218L || anyDuplicated(predictions$CodCanton)) {
    stop(
      "The OOF predictions for repeat ", repeat_id, " and scenario ",
      scenario_id, " do not contain exactly one row per canton."
    )
  }

  prediction_path <- file.path(
    config$output_root, "predictions", paste0("oof__", combo_id, ".csv")
  )
  readr::write_csv(predictions, prediction_path)

  metrics <- overall_metrics(predictions, "repeated_nested_spatial_oof") |>
    dplyr::mutate(
      repetition = repeat_id,
      seed = repeat_seed,
      scenario = scenario_id,
      model = "xgboost",
      design = "full",
      .before = 1L
    )
  readr::write_csv(
    metrics,
    file.path(config$output_root, "metrics", paste0("overall__", combo_id, ".csv"))
  )
  readr::write_csv(
    dplyr::bind_rows(best_parts),
    file.path(config$output_root, "tuning", paste0("best_all_outer_folds__", combo_id, ".csv"))
  )
  predictions
}

load_or_run_predictions <- function(scenario_input, membership, repeat_output,
                                    repeat_id, repeat_seed, scenario_id, overwrite) {
  scenario_output <- file.path(repeat_output, scenario_id)
  prediction_path <- file.path(
    scenario_output, "predictions", "oof__xgboost__full.csv"
  )

  if (!overwrite && file.exists(prediction_path)) {
    message("Reusing ", prediction_path)
    predictions <- readr::read_csv(
      prediction_path,
      show_col_types = FALSE,
      progress = FALSE
    )
    required_columns <- c(
      "repetition", "seed", "scenario", "CodCanton", "fold", "truth", "estimate"
    )
    if (
      !all(required_columns %in% names(predictions)) ||
        nrow(predictions) != 218L ||
        anyDuplicated(predictions$CodCanton)
    ) {
      stop("Existing predictions are incomplete or invalid: ", prediction_path)
    }
    saved_repetitions <- unique(predictions$repetition)
    saved_seeds <- unique(predictions$seed)
    saved_scenarios <- unique(predictions$scenario)
    if (
      length(saved_repetitions) != 1L ||
        as.integer(saved_repetitions) != as.integer(repeat_id) ||
        length(saved_seeds) != 1L ||
        as.integer(saved_seeds) != as.integer(repeat_seed) ||
        length(saved_scenarios) != 1L ||
        saved_scenarios != scenario_id
    ) {
      stop(
        "Existing predictions do not match the requested repetition, seed, or scenario: ",
        prediction_path,
        ". Set REPEATED_CV_OVERWRITE=true to replace them."
      )
    }
    return(predictions)
  }

  config <- scenario_input$config
  config$output_root <- scenario_output
  run_repeated_oof(
    spatial_data = scenario_input$spatial_data,
    frames = scenario_input$frames,
    membership = membership,
    config = config,
    repeat_id = repeat_id,
    repeat_seed = repeat_seed,
    scenario_id = scenario_id
  )
}

build_paired_results <- function(all_predictions, equivalence_margin_pp) {
  comparison_lookup <- tibble::tribble(
    ~augmented_scenario, ~comparison,
    "baseline_mobile_pca_mean", "Mobility factor",
    "baseline_mobile_mean", "38 mobility indicators"
  )

  administrative <- all_predictions |>
    dplyr::filter(.data$scenario == "baseline") |>
    dplyr::select(
      "repetition", "seed", "CodCanton", "fold",
      truth_administrative = "truth",
      estimate_administrative = "estimate"
    )

  paired_parts <- lapply(seq_len(nrow(comparison_lookup)), function(i) {
    augmented_scenario <- comparison_lookup$augmented_scenario[[i]]
    comparison_label <- comparison_lookup$comparison[[i]]
    augmented <- all_predictions |>
      dplyr::filter(.data$scenario == augmented_scenario) |>
      dplyr::select(
        "repetition", "seed", "CodCanton", "fold",
        truth_augmented = "truth",
        estimate_augmented = "estimate"
      )

    paired <- dplyr::inner_join(
      administrative,
      augmented,
      by = c("repetition", "seed", "CodCanton", "fold")
    )
    if (
      nrow(paired) != nrow(administrative) ||
        !isTRUE(all.equal(
          paired$truth_administrative,
          paired$truth_augmented,
          tolerance = 1e-12
        ))
    ) {
      stop("The paired predictions are not aligned for: ", comparison_label)
    }

    paired |>
      dplyr::transmute(
        repetition = .data$repetition,
        seed = .data$seed,
        comparison = comparison_label,
        augmented_scenario = augmented_scenario,
        CodCanton = .data$CodCanton,
        fold = .data$fold,
        truth = .data$truth_administrative,
        estimate_administrative = .data$estimate_administrative,
        estimate_augmented = .data$estimate_augmented,
        error_administrative_pp = 100 * (
          .data$estimate_administrative - .data$truth_administrative
        ),
        error_augmented_pp = 100 * (
          .data$estimate_augmented - .data$truth_administrative
        )
      ) |>
      dplyr::mutate(
        squared_loss_administrative_pp2 = .data$error_administrative_pp^2,
        squared_loss_augmented_pp2 = .data$error_augmented_pp^2,
        paired_squared_loss_difference_pp2 =
          .data$squared_loss_augmented_pp2 - .data$squared_loss_administrative_pp2,
        absolute_loss_administrative_pp = abs(.data$error_administrative_pp),
        absolute_loss_augmented_pp = abs(.data$error_augmented_pp),
        paired_absolute_loss_difference_pp =
          .data$absolute_loss_augmented_pp - .data$absolute_loss_administrative_pp
      )
  })

  paired_losses <- dplyr::bind_rows(paired_parts)
  repeat_metrics <- paired_losses |>
    dplyr::group_by(
      .data$comparison, .data$augmented_scenario, .data$repetition, .data$seed
    ) |>
    dplyr::summarise(
      n_cantons = dplyr::n(),
      rmse_administrative_pp = sqrt(mean(.data$squared_loss_administrative_pp2)),
      rmse_augmented_pp = sqrt(mean(.data$squared_loss_augmented_pp2)),
      delta_rmse_pp = .data$rmse_augmented_pp - .data$rmse_administrative_pp,
      mse_administrative_pp2 = mean(.data$squared_loss_administrative_pp2),
      mse_augmented_pp2 = mean(.data$squared_loss_augmented_pp2),
      delta_mean_squared_loss_pp2 = mean(.data$paired_squared_loss_difference_pp2),
      mae_administrative_pp = mean(.data$absolute_loss_administrative_pp),
      mae_augmented_pp = mean(.data$absolute_loss_augmented_pp),
      delta_mae_pp = .data$mae_augmented_pp - .data$mae_administrative_pp,
      share_cantons_with_lower_squared_loss = mean(
        .data$paired_squared_loss_difference_pp2 < 0
      ),
      .groups = "drop"
    )

  fold_metrics <- paired_losses |>
    dplyr::group_by(
      .data$comparison, .data$augmented_scenario,
      .data$repetition, .data$seed, .data$fold
    ) |>
    dplyr::summarise(
      n_cantons = dplyr::n(),
      rmse_administrative_pp = sqrt(mean(.data$squared_loss_administrative_pp2)),
      rmse_augmented_pp = sqrt(mean(.data$squared_loss_augmented_pp2)),
      delta_rmse_pp = .data$rmse_augmented_pp - .data$rmse_administrative_pp,
      delta_mean_squared_loss_pp2 = mean(.data$paired_squared_loss_difference_pp2),
      .groups = "drop"
    )

  summary <- repeat_metrics |>
    dplyr::group_by(.data$comparison, .data$augmented_scenario) |>
    dplyr::summarise(
      n_repeats = dplyr::n(),
      n_cantons_per_repeat = dplyr::first(.data$n_cantons),
      mean_rmse_administrative_pp = mean(.data$rmse_administrative_pp),
      mean_rmse_augmented_pp = mean(.data$rmse_augmented_pp),
      mean_delta_rmse_pp = mean(.data$delta_rmse_pp),
      sd_delta_rmse_pp = stats::sd(.data$delta_rmse_pp),
      median_delta_rmse_pp = stats::median(.data$delta_rmse_pp),
      partition_interval_low_pp = unname(
        stats::quantile(.data$delta_rmse_pp, 0.025, type = 8)
      ),
      partition_interval_high_pp = unname(
        stats::quantile(.data$delta_rmse_pp, 0.975, type = 8)
      ),
      min_delta_rmse_pp = min(.data$delta_rmse_pp),
      max_delta_rmse_pp = max(.data$delta_rmse_pp),
      share_repeats_improved = mean(.data$delta_rmse_pp < 0),
      mean_delta_squared_loss_pp2 = mean(.data$delta_mean_squared_loss_pp2),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      equivalence_margin_pp = equivalence_margin_pp,
      partition_assessment = dplyr::case_when(
        .data$partition_interval_high_pp < -equivalence_margin_pp ~
          "Improvement exceeds the practical margin",
        .data$partition_interval_low_pp > equivalence_margin_pp ~
          "Deterioration exceeds the practical margin",
        .data$partition_interval_low_pp >= -equivalence_margin_pp &
          .data$partition_interval_high_pp <= equivalence_margin_pp ~
          "Within the practical margin across tested partitions",
        TRUE ~ "Partition evidence is inconclusive relative to the margin"
      )
    ) |>
    dplyr::arrange(
      match(.data$comparison, c("Mobility factor", "38 mobility indicators"))
    )

  list(
    paired_losses = paired_losses,
    repeat_metrics = repeat_metrics,
    fold_metrics = fold_metrics,
    summary = summary
  )
}

write_appendix_outputs <- function(results, output_root, equivalence_margin_pp) {
  results_dir <- file.path(output_root, "results")
  figure_dir <- file.path(output_root, "figures")
  table_dir <- file.path(output_root, "tables")
  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

  readr::write_csv(
    results$paired_losses,
    file.path(results_dir, "paired_canton_losses.csv")
  )
  readr::write_csv(
    results$repeat_metrics,
    file.path(results_dir, "paired_repeat_metrics.csv")
  )
  readr::write_csv(
    results$fold_metrics,
    file.path(results_dir, "paired_fold_metrics.csv")
  )
  readr::write_csv(
    results$summary,
    file.path(results_dir, "paired_summary.csv")
  )

  plot_data <- results$repeat_metrics |>
    dplyr::mutate(
      comparison = factor(
        .data$comparison,
        levels = c("Mobility factor", "38 mobility indicators")
      )
    )
  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$comparison, y = .data$delta_rmse_pp)
  ) +
    ggplot2::geom_hline(
      yintercept = c(-equivalence_margin_pp, equivalence_margin_pp),
      linetype = "dotted",
      color = "grey45",
      linewidth = 0.45
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "black",
      linewidth = 0.55
    ) +
    ggplot2::geom_boxplot(
      width = 0.52,
      outlier.shape = NA,
      fill = "#9ECAE1",
      color = "#08519C",
      alpha = 0.65
    ) +
    ggplot2::geom_point(
      position = ggplot2::position_jitter(width = 0.08, height = 0, seed = 2026),
      size = 2.1,
      alpha = 0.78,
      color = "#08306B"
    ) +
    ggplot2::labs(
      x = NULL,
      y = expression(Delta * " OOF RMSE (percentage points)"),
      caption = paste0(
        "Negative values favor mobility. Dotted lines show the +/-",
        format_number(equivalence_margin_pp), " pp practical margin."
      )
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(hjust = 0)
    )

  ggplot2::ggsave(
    file.path(figure_dir, "paired_delta_rmse.png"),
    plot,
    width = 7.2,
    height = 4.3,
    dpi = 300,
    bg = "white"
  )
  ggplot2::ggsave(
    file.path(figure_dir, "paired_delta_rmse.pdf"),
    plot,
    width = 7.2,
    height = 4.3,
    device = grDevices::cairo_pdf
  )

  table_rows <- vapply(seq_len(nrow(results$summary)), function(i) {
    row <- results$summary[i, ]
    interval <- paste0(
      "[", format_number(row$partition_interval_low_pp), ", ",
      format_number(row$partition_interval_high_pp), "]"
    )
    paste0(
      latex_escape(row$comparison), " & ",
      row$n_repeats, " & ",
      format_number(row$mean_rmse_administrative_pp), " & ",
      format_number(row$mean_rmse_augmented_pp), " & ",
      format_number(row$mean_delta_rmse_pp), " & ",
      interval, " & ",
      format_number(100 * row$share_repeats_improved, digits = 0L), "\\% & ",
      latex_escape(row$partition_assessment), " \\\\"
    )
  }, character(1))

  table_lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\small",
    "\\resizebox{\\textwidth}{!}{%",
    "\\begin{tabular}{lrrrrcrp{5.3cm}}",
    "\\toprule",
    paste0(
      "Mobility input & Repeats & Admin. RMSE & Augmented RMSE & ",
      "$\\Delta$RMSE & 95\\% partition interval & Improved & Assessment \\\\"
    ),
    "\\midrule",
    table_rows,
    "\\bottomrule",
    "\\end{tabular}%",
    "}",
    paste0(
      "\\caption{Paired repeated spatial cross-validation for the Full XGBoost ",
      "specification. $\\Delta$RMSE is augmented minus administrative-only RMSE, ",
      "so negative values favor mobility. The empirical interval is the 2.5th--97.5th ",
      "percentile range across spatial partitions and measures sensitivity to partition ",
      "choice; it is not a population-sampling confidence interval.}"
    ),
    "\\label{tab:repeated-spatial-cv}",
    paste0(
      "\\begin{minipage}{0.96\\textwidth}\\footnotesize ",
      "\\textit{Notes:} Administrative-only and augmented models use identical held-out ",
      "cantons within every repetition. All preprocessing and hyperparameter tuning are ",
      "re-estimated inside each outer training sample. The practical equivalence margin is ",
      "$\\pm ", format_number(equivalence_margin_pp), "$ RMSE percentage points."
    ),
    "\\end{minipage}",
    "\\end{table}"
  )
  writeLines(
    table_lines,
    file.path(table_dir, "repeated_spatial_cv.tex"),
    useBytes = TRUE
  )

  appendix_lines <- c(
    "\\subsection{Sensitivity to repeated spatial partitions}",
    "\\label{app:repeated-spatial-cv}",
    paste0(
      "To assess whether the comparison between administrative-only and mobility-augmented ",
      "models depends on a particular spatial partition, we repeat the nested spatial ",
      "cross-validation procedure using different clustering seeds. Within each repetition, ",
      "the Full XGBoost specifications use identical outer assessment cantons, while all ",
      "preprocessing and hyperparameter tuning are re-estimated using only the corresponding ",
      "training observations. We compare the administrative specification with both the ",
      "mobility-factor and 38-indicator mobility specifications using paired out-of-fold ",
      "prediction losses."
    ),
    "",
    "\\input{output_tidymodels/cv_partitioned/tables/repeated_spatial_cv.tex}",
    "",
    "\\begin{figure}[!htbp]",
    "\\centering",
    paste0(
      "\\includegraphics[width=0.76\\textwidth]",
      "{output_tidymodels/cv_partitioned/figures/paired_delta_rmse.pdf}"
    ),
    paste0(
      "\\caption{Paired out-of-fold RMSE differences across repeated spatial partitions. ",
      "Negative values indicate lower RMSE after adding mobility; dotted lines denote the ",
      "practical equivalence margin.}"
    ),
    "\\label{fig:repeated-spatial-cv}",
    "\\end{figure}"
  )
  writeLines(
    appendix_lines,
    file.path(table_dir, "repeated_spatial_cv_appendix.tex"),
    useBytes = TRUE
  )
  invisible(TRUE)
}

main <- function() {
  repo_root <- find_project_root()
  source(file.path(repo_root, "new code", "R", "load_all.R"), local = globalenv())
  source_new_pipeline(repo_root)

  scenarios <- c(
    "baseline",
    "baseline_mobile_pca_mean",
    "baseline_mobile_mean"
  )
  base_config <- pipeline_config(repo_root, "baseline")
  base_config$designs <- "full"
  base_config$models <- "xgboost"
  base_config$continue_on_error <- FALSE
  check_pipeline_dependencies(base_config)
  attach_processing_packages()

  n_repeats <- env_int("REPEATED_CV_REPEATS", 10L)
  base_seed <- env_int("REPEATED_CV_BASE_SEED", 123L)
  seed_stride <- env_int("REPEATED_CV_SEED_STRIDE", 10000L)
  equivalence_margin_pp <- env_double("PRACTICAL_EQUIVALENCE_MARGIN_PP", 0.50)
  overwrite <- env_flag("REPEATED_CV_OVERWRITE", FALSE)
  validate_only <- env_flag("REPEATED_CV_VALIDATE_ONLY", FALSE)
  output_override <- Sys.getenv("REPEATED_CV_OUTPUT_ROOT", unset = "")
  output_root <- if (nzchar(output_override)) {
    normalizePath(output_override, winslash = "/", mustWork = FALSE)
  } else {
    file.path(repo_root, "output_tidymodels", "cv_partitioned")
  }

  if (n_repeats < 2L && !validate_only) {
    stop("REPEATED_CV_REPEATS must be at least 2 for a repeated analysis.")
  }
  if (equivalence_margin_pp <= 0) {
    stop("PRACTICAL_EQUIVALENCE_MARGIN_PP must be positive.")
  }
  if (!nzchar(Sys.getenv("PRACTICAL_EQUIVALENCE_MARGIN_PP", unset = ""))) {
    warning(
      "Using the provisional practical margin of 0.50 RMSE percentage points. ",
      "Justify this value substantively or set PRACTICAL_EQUIVALENCE_MARGIN_PP before the final run."
    )
  }
  if (n_repeats < 10L && !validate_only) {
    warning("Fewer than 10 repetitions provide a limited partition-sensitivity interval.")
  }

  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
  prepared_root <- file.path(output_root, "prepared")
  dir.create(prepared_root, recursive = TRUE, showWarnings = FALSE)

  message("Preparing the three aligned data scenarios...")
  scenario_inputs <- stats::setNames(
    lapply(scenarios, function(scenario_id) {
      prepare_scenario(repo_root, scenario_id, prepared_root)
    }),
    scenarios
  )
  validate_scenario_alignment(scenario_inputs, scenarios)
  validate_workflows(scenario_inputs, scenarios)

  validation_config <- scenario_inputs[["baseline"]]$config
  validation_config$seed <- base_seed
  validation_membership <- make_repeat_membership(
    scenario_inputs[["baseline"]]$spatial_data,
    validation_config,
    repeat_id = 1L,
    repeat_seed = base_seed
  )
  if (validate_only) {
    message(
      "Validation complete: ", nrow(validation_membership),
      " cantons assigned to ", dplyr::n_distinct(validation_membership$outer_fold),
      " spatial folds. No models were fitted."
    )
    return(invisible(TRUE))
  }

  run_manifest <- tibble::tibble(
    repetition = seq_len(n_repeats),
    seed = base_seed + (seq_len(n_repeats) - 1L) * seed_stride,
    outer_folds = base_config$outer_v,
    inner_folds = base_config$inner_v,
    bayes_initial = base_config$bayes_initial,
    bayes_iter = base_config$bayes_iter,
    practical_equivalence_margin_pp = equivalence_margin_pp
  )
  readr::write_csv(run_manifest, file.path(output_root, "run_manifest.csv"))
  writeLines(
    utils::capture.output(utils::sessionInfo()),
    file.path(output_root, "session_info.txt")
  )

  prediction_parts <- list()
  membership_parts <- vector("list", n_repeats)
  part_index <- 0L

  for (repeat_id in seq_len(n_repeats)) {
    repeat_seed <- run_manifest$seed[[repeat_id]]
    repeat_name <- sprintf("repeat_%02d", repeat_id)
    repeat_output <- file.path(output_root, "repeats", repeat_name)
    dir.create(repeat_output, recursive = TRUE, showWarnings = FALSE)

    repeat_config <- scenario_inputs[["baseline"]]$config
    repeat_config$seed <- repeat_seed
    membership <- make_repeat_membership(
      scenario_inputs[["baseline"]]$spatial_data,
      repeat_config,
      repeat_id,
      repeat_seed
    )
    membership_parts[[repeat_id]] <- membership
    readr::write_csv(
      membership,
      file.path(repeat_output, "outer_spatial_fold_membership.csv")
    )

    message(
      "\n=== Repeated spatial CV ", repeat_id, "/", n_repeats,
      " (seed ", repeat_seed, ") ==="
    )
    for (scenario_id in scenarios) {
      part_index <- part_index + 1L
      prediction_parts[[part_index]] <- load_or_run_predictions(
        scenario_input = scenario_inputs[[scenario_id]],
        membership = membership,
        repeat_output = repeat_output,
        repeat_id = repeat_id,
        repeat_seed = repeat_seed,
        scenario_id = scenario_id,
        overwrite = overwrite
      )
    }
  }

  all_memberships <- dplyr::bind_rows(membership_parts)
  readr::write_csv(
    all_memberships,
    file.path(output_root, "outer_spatial_fold_memberships.csv")
  )
  membership_signatures <- all_memberships |>
    dplyr::arrange(.data$repetition, .data$CodCanton) |>
    dplyr::group_by(.data$repetition, .data$seed) |>
    dplyr::summarise(
      signature = paste(.data$outer_fold, collapse = "|"),
      .groups = "drop"
    )
  if (anyDuplicated(membership_signatures$signature)) {
    warning("At least two repetitions produced identical outer-fold assignments.")
  }

  all_predictions <- dplyr::bind_rows(prediction_parts)
  readr::write_csv(
    all_predictions,
    file.path(output_root, "all_oof_predictions.csv")
  )
  paired_results <- build_paired_results(
    all_predictions,
    equivalence_margin_pp
  )
  write_appendix_outputs(
    paired_results,
    output_root,
    equivalence_margin_pp
  )

  message("\nRepeated spatial-CV analysis complete.")
  message("Results: ", file.path(output_root, "results", "paired_summary.csv"))
  message(
    "Appendix: ",
    file.path(output_root, "tables", "repeated_spatial_cv_appendix.tex")
  )
  invisible(TRUE)
}

if (sys.nframe() == 0L) {
  main()
}
