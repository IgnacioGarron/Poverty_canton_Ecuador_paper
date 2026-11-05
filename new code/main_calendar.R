#!/usr/bin/env Rscript

# Ejecutar desde `new code/`:
#   Rscript main_calendar.R
# La estimación completa ejecuta seis modelos XGBoost con CV espacial anidada
# y genera boxplots de RMSE OOF y sesgo OOF por fold espacial.
# Para validar únicamente datos, calendario y folds:
#   CALENDAR_PREPARE_ONLY=true Rscript main_calendar.R

new_code_root <- normalizePath(getwd(), mustWork = TRUE)
repo_root <- normalizePath(file.path(new_code_root, ".."), mustWork = TRUE)
source(file.path(new_code_root, "R", "load_all.R"))
source_new_pipeline(repo_root)
source(file.path(new_code_root, "new_calendar.R"))

suppressWarnings(suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(sf)
  library(tidyr)
  library(tidymodels)
}))

scenario_source <- "baseline_mobile_pca_mean"
calendar_root <- file.path(repo_root, "output_tidymodels", "calendar")
dir.create(calendar_root, recursive = TRUE, showWarnings = FALSE)

calendar <- build_new_calendar(repo_root, write_output = TRUE)
base_config <- pipeline_config(repo_root, scenario_source)
base_config$models <- "xgboost"
base_config$designs <- "full"
validate_config(base_config)
check_pipeline_dependencies(base_config)
attach_processing_packages()

processed <- load_processing_output(base_config, scenario_spec(scenario_source))
frames_all <- build_model_frames(processed, calendar_root)
variable_map <- read_csv(
  file.path(calendar_root, "variable_name_map.csv"),
  show_col_types = FALSE,
  progress = FALSE
)

admin_calendar <- calendar |>
  filter(model_feature != "mobility_PC1") |>
  mutate(variable_norm = normalize_calendar_name(variable_original))
variable_map <- variable_map |>
  mutate(variable_norm = normalize_calendar_name(original_name))
admin_calendar <- admin_calendar |>
  left_join(
    variable_map |>
      select(variable_norm, predictor_name = model_name),
    by = "variable_norm"
  )
if (anyNA(admin_calendar$predictor_name)) {
  stop(
    "No se encontraron predictores para: ",
    paste(admin_calendar$variable_original[is.na(admin_calendar$predictor_name)], collapse = ", ")
  )
}
if (anyDuplicated(admin_calendar$predictor_name)) {
  stop("El calendario administrativo contiene predictores duplicados.")
}
if (length(frames_all$mobile_predictor_names) != 38L) {
  stop("El escenario con PCA móvil debe contener 38 predictores móviles crudos.")
}

fold_file <- file.path(
  repo_root,
  "output_tidymodels",
  scenario_source,
  "folds",
  "outer_spatial_folds.rds"
)
if (!file.exists(fold_file)) {
  stop("No existen los folds espaciales de referencia: ", fold_file)
}
outer_folds <- readRDS(fold_file)
fold_codes <- fold_membership(outer_folds)$CodCanton
if (!setequal(as.character(fold_codes), as.character(frames_all$df_2022$CodCanton))) {
  stop("Los folds de referencia no contienen los mismos cantones del nuevo calendario.")
}

quarter_levels <- c(Q1 = 1L, Q2 = 2L, Q4 = 4L)
quarter_number <- function(x) unname(quarter_levels[x])

subset_calendar_frames <- function(frames, predictor_names, include_pca) {
  selected <- predictor_names
  mobile_names <- character(0)
  if (isTRUE(include_pca)) {
    mobile_names <- frames$mobile_predictor_names
    selected <- unique(c(selected, mobile_names))
  }
  missing_predictors <- setdiff(selected, frames$predictor_names)
  if (length(missing_predictors)) {
    stop("Faltan predictores en los frames: ", paste(missing_predictors, collapse = ", "))
  }

  list(
    df_2022 = frames$df_2022 |>
      select(all_of(c("CodCanton", "Canton", "Y", "poverty_group", selected))),
    df_2023 = frames$df_2023 |>
      select(all_of(c("CodCanton", "Canton", "poverty_group", selected))),
    predictor_names = selected,
    mobile_predictor_names = mobile_names,
    y_2023_mef = frames$y_2023_mef,
    y_2023_enemdu = frames$y_2023_enemdu
  )
}

run_calendar_fit <- function(quarter, include_pca, frames) {
  comparison_id <- if (include_pca) "with_pca" else "without_pca"
  run_output <- file.path(calendar_root, comparison_id, quarter)
  create_output_dirs(run_output)
  metrics_file <- file.path(run_output, "metrics", "overall__xgboost__full.csv")
  oof_file <- file.path(run_output, "predictions", "oof__xgboost__full.csv")
  overwrite <- env_flag("CALENDAR_OVERWRITE", FALSE)

  if (!overwrite && file.exists(metrics_file) && file.exists(oof_file)) {
    message("Reutilizando resultados: ", comparison_id, " / ", quarter)
    return(read_csv(metrics_file, show_col_types = FALSE, progress = FALSE) |>
      filter(evaluation == "nested_spatial_oof"))
  }

  config <- pipeline_config(
    repo_root,
    if (include_pca) "baseline_mobile_pca_mean" else "baseline"
  )
  config$output_root <- run_output
  config$models <- "xgboost"
  config$designs <- "full"
  config$continue_on_error <- FALSE
  validate_config(config)
  saveRDS(config, file.path(run_output, "run_config.rds"))
  writeLines(
    utils::capture.output(utils::sessionInfo()),
    file.path(run_output, "session_info.txt")
  )

  spatial_data <- join_canton_geometry(frames$df_2022, repo_root, run_output)
  result <- run_nested_spatial_combo(
    spatial_data = spatial_data,
    frames = frames,
    outer_folds = outer_folds,
    design = "full",
    model_name = "xgboost",
    config = config
  )
  result$overall |>
    filter(evaluation == "nested_spatial_oof")
}

read_calendar_fold_metrics <- function(quarter, comparison) {
  prediction_file <- file.path(
    calendar_root,
    comparison,
    quarter,
    "predictions",
    "oof__xgboost__full.csv"
  )
  if (!file.exists(prediction_file)) {
    stop("No existen predicciones OOF para ", comparison, " / ", quarter)
  }

  predictions <- read_csv(
    prediction_file,
    show_col_types = FALSE,
    progress = FALSE
  )
  required <- c("fold", "truth", "estimate")
  missing_columns <- setdiff(required, names(predictions))
  if (length(missing_columns)) {
    stop(
      "Faltan columnas en ", prediction_file, ": ",
      paste(missing_columns, collapse = ", ")
    )
  }

  predictions |>
    mutate(error_pp = 100 * (estimate - truth)) |>
    group_by(fold) |>
    summarise(
      n_cantons = n(),
      rmse_oof_pp = sqrt(mean(error_pp^2)),
      bias_oof_pp = mean(error_pp),
      .groups = "drop"
    ) |>
    mutate(
      comparison = .env$comparison,
      quarter = .env$quarter,
      .before = 1L
    )
}

run_manifest <- bind_rows(lapply(names(quarter_levels), function(quarter) {
  cumulative_admin <- admin_calendar |>
    filter(quarter_number(release_quarter) <= quarter_levels[[quarter]])
  bind_rows(
    tibble::tibble(
      comparison = "without_pca",
      quarter = quarter,
      pca_factor = FALSE,
      administrative_variables = nrow(cumulative_admin),
      mobile_raw_variables = 0L,
      model_predictors_after_recipe = nrow(cumulative_admin)
    ),
    tibble::tibble(
      comparison = "with_pca",
      quarter = quarter,
      pca_factor = TRUE,
      administrative_variables = nrow(cumulative_admin),
      mobile_raw_variables = length(frames_all$mobile_predictor_names),
      model_predictors_after_recipe = nrow(cumulative_admin) + 1L
    )
  )
}))
write_csv(run_manifest, file.path(calendar_root, "calendar_run_manifest.csv"))

if (env_flag("CALENDAR_PREPARE_ONLY", FALSE)) {
  message("Preparación verificada; no se estimaron modelos (CALENDAR_PREPARE_ONLY=true).")
} else {
  metrics_parts <- list()
  fold_metrics_parts <- list()
  for (quarter in names(quarter_levels)) {
    cumulative_admin <- admin_calendar |>
      filter(quarter_number(release_quarter) <= quarter_levels[[quarter]])
    admin_predictors <- cumulative_admin$predictor_name

    for (include_pca in c(FALSE, TRUE)) {
      comparison_id <- if (include_pca) "with_pca" else "without_pca"
      message("\n=== Calendar: ", comparison_id, " / ", quarter, " ===")
      stage_frames <- subset_calendar_frames(
        frames_all,
        predictor_names = admin_predictors,
        include_pca = include_pca
      )
      metric <- run_calendar_fit(quarter, include_pca, stage_frames) |>
        mutate(comparison = comparison_id, quarter = quarter, .before = 1L)
      metrics_parts[[paste(comparison_id, quarter, sep = "__")]] <- metric
      fold_metrics_parts[[paste(comparison_id, quarter, sep = "__")]] <-
        read_calendar_fold_metrics(quarter, comparison_id)
    }
  }

  metrics_oof <- bind_rows(metrics_parts) |>
    mutate(
      comparison = factor(
        comparison,
        levels = c("without_pca", "with_pca")
      ),
      quarter = factor(quarter, levels = names(quarter_levels))
    ) |>
    arrange(comparison, quarter) |>
    select(
      comparison, quarter, evaluation, n, rmse, rmse_pp, mae, mae_pp,
      r_squared, mean_signed_error, mean_signed_error_pp
    )
  write_csv(
    metrics_oof,
    file.path(calendar_root, "calendar_oof_overall_metrics.csv")
  )

  fold_metrics <- bind_rows(fold_metrics_parts) |>
    mutate(
      comparison = factor(
        comparison,
        levels = c("without_pca", "with_pca")
      ),
      quarter = factor(quarter, levels = names(quarter_levels))
    ) |>
    arrange(comparison, quarter, fold)

  expected_rows <- 2L * length(quarter_levels) * nrow(outer_folds)
  if (nrow(fold_metrics) != expected_rows ||
      any(!is.finite(fold_metrics$rmse_oof_pp)) ||
      any(!is.finite(fold_metrics$bias_oof_pp))) {
    stop("Las métricas por fold no cubren las seis comparaciones esperadas.")
  }
  fold_coverage <- fold_metrics |>
    count(comparison, quarter, name = "n_folds")
  if (any(fold_coverage$n_folds != nrow(outer_folds))) {
    stop("Alguna comparación no contiene todos los folds espaciales.")
  }

  write_csv(
    fold_metrics,
    file.path(calendar_root, "calendar_oof_fold_metrics.csv")
  )

  metric_levels <- c("OOS RMSE (pp)", "OOS Bias (pp)")
  plot_data <- fold_metrics |>
    pivot_longer(
      cols = c(rmse_oof_pp, bias_oof_pp),
      names_to = "metric_id",
      values_to = "value_pp"
    ) |>
    mutate(
      comparison_label = factor(
        comparison,
        levels = c("without_pca", "with_pca"),
        labels = c("Without mobile data", "With mobile data")
      ),
      metric = factor(
        metric_id,
        levels = c("rmse_oof_pp", "bias_oof_pp"),
        labels = metric_levels
      )
    )

  figure_dir <- file.path(calendar_root, "figures")
  dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
  figure_base <- file.path(figure_dir, "calendar_oof_boxplot")
  write_csv(
    plot_data |>
      transmute(
        scenario = scenario_source,
        comparison = as.character(comparison),
        comparison_label = as.character(comparison_label),
        quarter = as.character(quarter),
        metric = as.character(metric),
        fold,
        n_cantons,
        value_pp
      ),
    paste0(figure_base, ".csv")
  )

  bias_zero <- plot_data |>
    filter(metric == "OOS Bias (pp)") |>
    distinct(metric) |>
    mutate(intercept = 0)
  calendar_plot <- ggplot(
    plot_data,
    aes(
      x = quarter,
      y = value_pp,
      fill = comparison_label,
      group = interaction(quarter, comparison_label)
    )
  ) +
    geom_hline(
      data = bias_zero,
      aes(yintercept = intercept),
      inherit.aes = FALSE,
      colour = "grey45",
      linewidth = 0.45,
      linetype = "dashed"
    ) +
    geom_boxplot(
      position = position_dodge(width = 0.75),
      width = 0.62,
      alpha = 0.60,
      outlier.shape = NA
    ) +
    geom_point(
      position = position_jitterdodge(
        jitter.width = 0.06,
        dodge.width = 0.75,
        seed = 2026
      ),
      shape = 21,
      size = 2.2,
      stroke = 0.35,
      colour = "white"
    ) +
    facet_wrap(vars(metric), ncol = 2L, scales = "free_y") +
    scale_fill_manual(
      values = c("Without mobile data" = "#0072B2", "With mobile data" = "#D55E00"),
      drop = FALSE
    ) +
    labs(
      x = "Calendar quarter",
      y = NULL,
      fill = NULL
    ) +
    theme_bw(base_size = 12) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.text = element_text(face = "bold")
    )

  ggsave(
    paste0(figure_base, ".png"),
    calendar_plot,
    width = 6,
    height = 4,
    dpi = 300,
    bg = "white"
  )
  message("Boxplot guardado en: ", paste0(figure_base, ".png"))
}
