#!/usr/bin/env Rscript

suppressWarnings(suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
}))

normalize_calendar_name <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  gsub("[^a-z0-9]+", "", x)
}

extract_pca_shap_importance <- function(model_file) {
  if (!file.exists(model_file)) {
    stop("No existe el workflow XGBoost FULL: ", model_file)
  }

  fitted_workflow <- readRDS(model_file)
  xgb_fit <- workflows::extract_fit_engine(fitted_workflow)
  predictors <- workflows::extract_mold(fitted_workflow)$predictors
  predictor_matrix <- as.matrix(predictors)
  shap_values <- predict(
    xgb_fit,
    newdata = xgboost::xgb.DMatrix(predictor_matrix),
    predcontrib = TRUE,
    approxcontrib = FALSE
  )

  feature_names <- colnames(shap_values)
  if (is.null(feature_names)) {
    feature_names <- c(colnames(predictor_matrix), "(Intercept)")
  }
  intercept_column <- which(feature_names == "(Intercept)")
  if (length(intercept_column) != 1L) {
    stop("No se pudo identificar una única columna de intercepto SHAP.")
  }

  shap_features <- shap_values[, -intercept_column, drop = FALSE]
  names_features <- feature_names[-intercept_column]
  importance <- tibble::tibble(
    model_feature = names_features,
    mean_abs_shap = colMeans(abs(shap_features)),
    mean_shap = colMeans(shap_features)
  ) |>
    arrange(desc(mean_abs_shap), model_feature) |>
    mutate(shap_rank = row_number())

  pca <- importance |>
    filter(model_feature == "mobility_PC1")
  if (nrow(pca) != 1L) {
    stop("El workflow no contiene exactamente un factor mobility_PC1.")
  }
  pca
}

old_calendar_assignments <- function(calendar_file) {
  if (!file.exists(calendar_file)) {
    stop("No existe el calendario histórico: ", calendar_file)
  }
  old <- read_excel(calendar_file, sheet = "Sheet1")
  required <- c("variable", "I", "II", "III", "IV")
  missing_columns <- setdiff(required, names(old))
  if (length(missing_columns)) {
    stop(
      "Faltan columnas en calendario.xlsx: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  third_quarter_flags <- suppressWarnings(as.numeric(old$III))
  if (any(replace(third_quarter_flags, is.na(third_quarter_flags), 0) != 0)) {
    stop("El calendario histórico contiene variables en III, pero la nueva tabla no incluye Q3.")
  }

  quarter_map <- c(I = "Q1", II = "Q2", IV = "Q4")
  assignments <- bind_rows(lapply(names(quarter_map), function(column) {
    flags <- suppressWarnings(as.numeric(old[[column]]))
    old |>
      filter(replace(flags, is.na(flags), 0) != 0) |>
      transmute(
        calendar_reference_variable = as.character(variable),
        variable_norm = normalize_calendar_name(variable),
        release_quarter = unname(quarter_map[[column]])
      )
  }))

  duplicated_variables <- assignments |>
    count(variable_norm) |>
    filter(n != 1L)
  if (nrow(duplicated_variables)) {
    stop("Hay variables con más de una asignación trimestral en calendario.xlsx.")
  }
  assignments
}

escape_latex <- function(x) {
  gsub("([#$%&_])", "\\\\\\1", as.character(x), perl = TRUE)
}

write_calendar_latex <- function(calendar, output_file) {
  required <- c("selection_order", "variable", "release_quarter")
  missing_columns <- setdiff(required, names(calendar))
  if (length(missing_columns)) {
    stop(
      "Faltan columnas para construir la tabla LaTeX: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  if (anyNA(calendar$variable) || any(!nzchar(calendar$variable))) {
    stop("Todos los nombres de variables para la tabla deben estar en inglés.")
  }

  allowed_quarters <- c("Q1", "Q2", "Q4")
  if (any(!calendar$release_quarter %in% allowed_quarters)) {
    stop("La tabla LaTeX solo admite Q1, Q2 y Q4.")
  }

  table_data <- calendar |>
    arrange(selection_order) |>
    transmute(
      variable = escape_latex(variable),
      Q1 = if_else(release_quarter == "Q1", "X", ""),
      Q2 = if_else(release_quarter == "Q2", "X", ""),
      Q4 = if_else(release_quarter == "Q4", "X", "")
    )

  table_rows <- apply(
    table_data,
    1L,
    function(row) {
      paste0(
        "        ", row[["variable"]], " & ", row[["Q1"]], " & ",
        row[["Q2"]], " & ", row[["Q4"]], " \\\\"
      )
    }
  )

  latex <- c(
    "\\begin{table}[!htbp]",
    "    \\centering",
    "    \\footnotesize",
    "    \\renewcommand{\\arraystretch}{1.05}",
    "    \\begin{tabular}{p{0.72\\textwidth}ccc}",
    "        \\toprule",
    "        \\textbf{Variable} & \\textbf{Q1} & \\textbf{Q2} & \\textbf{Q4} \\\\ \\midrule",
    table_rows,
    "        \\bottomrule",
    "    \\end{tabular}",
    paste0(
      "    \\caption{Calendar of variables selected from XGBoost FULL using ",
      "mean absolute SHAP. An X indicates the quarter in which each variable ",
      "becomes available.}"
    ),
    "    \\label{tab:calendar_variables_xgboost_full}",
    "\\end{table}"
  )
  writeLines(latex, output_file, useBytes = TRUE)
  invisible(output_file)
}

build_new_calendar <- function(repo_root, write_output = TRUE) {
  scenario <- "baseline_mobile_pca_mean"
  output_root <- file.path(repo_root, "output_tidymodels")
  calendar_output <- file.path(output_root, "calendar")
  importance_file <- file.path(
    output_root,
    "figures",
    paste0("variable_importance_xgboost_full_", scenario, ".csv")
  )
  old_calendar_file <- file.path(
    repo_root,
    "input",
    "Data",
    "Covariables_Actualizadas_2010_enviar",
    "calendario.xlsx"
  )
  model_file <- file.path(
    output_root,
    scenario,
    "models",
    "xgboost__full.rds"
  )

  if (!file.exists(importance_file)) {
    stop(
      "No existe el ranking SHAP requerido: ", importance_file,
      ". Ejecute primero variable_importance_XGboost.R."
    )
  }
  top20 <- read_csv(importance_file, show_col_types = FALSE, progress = FALSE)
  required_importance <- c(
    "scenario", "model", "design", "rank", "variable_original", "variable",
    "model_feature", "mean_abs_shap", "mean_abs_shap_pp", "mean_shap", "mean_shap_pp"
  )
  missing_importance <- setdiff(required_importance, names(top20))
  if (length(missing_importance)) {
    stop(
      "Faltan columnas en el ranking SHAP: ",
      paste(missing_importance, collapse = ", ")
    )
  }
  top20 <- top20 |>
    filter(
      scenario == "baseline_mobile_pca_mean",
      model == "xgboost",
      design == "full"
    ) |>
    arrange(rank) |>
    slice_head(n = 20L)
  if (nrow(top20) != 20L || anyDuplicated(top20$model_feature)) {
    stop("El ranking debe contener exactamente 20 variables distintas.")
  }
  if ("mobility_PC1" %in% top20$model_feature) {
    stop(
      "mobility_PC1 ya pertenece al top 20; no puede añadirse como la variable adicional 21."
    )
  }

  historical <- old_calendar_assignments(old_calendar_file)
  # Dos variables del nuevo top 20 no estaban en el archivo histórico. Se les
  # asigna el trimestre de otra variable de la misma operación estadística.
  inferred_assignments <- tibble::tribble(
    ~variable_original, ~calendar_reference_variable, ~release_quarter,
    "ParticipacionVentasExplotación de minas y canteras",
    "ParticipacionVentasAgricultura, ganadería, silvicultura y pesca", "Q1",
    "Porcentajenacprematuromoderadomujertotalmadre",
    "Porcentajenacprematuromoderadomujermujermadre", "Q2"
  ) |>
    mutate(variable_norm = normalize_calendar_name(variable_original)) |>
    select(-variable_original)

  pca_importance <- extract_pca_shap_importance(model_file)
  top_calendar <- top20 |>
    transmute(
      selection_order = as.integer(rank),
      shap_rank = as.integer(rank),
      variable = as.character(variable),
      variable_original = as.character(variable_original),
      model_feature = as.character(model_feature),
      mean_abs_shap,
      mean_abs_shap_pp,
      mean_shap,
      mean_shap_pp,
      selection_basis = "top_20_mean_abs_shap",
      variable_norm = normalize_calendar_name(variable_original)
    ) |>
    left_join(historical, by = "variable_norm") |>
    left_join(
      inferred_assignments,
      by = "variable_norm",
      suffix = c("", "_inferred")
    ) |>
    mutate(
      calendar_reference_variable = coalesce(
        calendar_reference_variable,
        calendar_reference_variable_inferred
      ),
      release_quarter = coalesce(release_quarter, release_quarter_inferred)
    ) |>
    select(-ends_with("_inferred"))

  unresolved <- top_calendar |>
    filter(is.na(release_quarter)) |>
    pull(variable_original)
  if (length(unresolved)) {
    stop(
      "No se pudo asignar trimestre a: ",
      paste(unresolved, collapse = ", ")
    )
  }

  pca_row <- tibble::tibble(
    selection_order = 21L,
    shap_rank = as.integer(pca_importance$shap_rank[[1]]),
    variable = "Mobility Factor (PCA Mean)",
    variable_original = "mobility_PC1",
    model_feature = "mobility_PC1",
    mean_abs_shap = pca_importance$mean_abs_shap[[1]],
    mean_abs_shap_pp = 100 * pca_importance$mean_abs_shap[[1]],
    mean_shap = pca_importance$mean_shap[[1]],
    mean_shap_pp = 100 * pca_importance$mean_shap[[1]],
    selection_basis = "additional_pca_mean_factor",
    variable_norm = normalize_calendar_name("mobility_PC1"),
    calendar_reference_variable = "pca",
    release_quarter = "Q1"
  )

  calendar <- bind_rows(top_calendar, pca_row) |>
    arrange(selection_order) |>
    mutate(
      scenario = scenario,
      Q1 = if_else(release_quarter == "Q1", 1L, NA_integer_),
      Q2 = if_else(release_quarter == "Q2", 1L, NA_integer_),
      Q4 = if_else(release_quarter == "Q4", 1L, NA_integer_),
      .before = 1L
    ) |>
    select(
      scenario, selection_order, shap_rank, variable, variable_original,
      model_feature, mean_abs_shap, mean_abs_shap_pp, mean_shap, mean_shap_pp,
      release_quarter, Q1, Q2, Q4, selection_basis,
      calendar_reference_variable
    )

  if (nrow(calendar) != 21L || anyDuplicated(calendar$model_feature)) {
    stop("El nuevo calendario no contiene 20 variables SHAP más un factor PCA único.")
  }
  if (!identical(sort(unique(calendar$release_quarter)), c("Q1", "Q2", "Q4"))) {
    stop("Las asignaciones del nuevo calendario deben cubrir Q1, Q2 y Q4.")
  }

  if (isTRUE(write_output)) {
    dir.create(calendar_output, recursive = TRUE, showWarnings = FALSE)
    write_csv(calendar, file.path(calendar_output, "new_calendar.csv"))
    write_calendar_latex(
      calendar,
      file.path(calendar_output, "calendar_variables_xgboost_full.tex")
    )
  }
  calendar
}

if (sys.nframe() == 0L) {
  repo_root <- normalizePath("..", mustWork = TRUE)
  new_calendar <- build_new_calendar(repo_root, write_output = TRUE)
  counts <- new_calendar |>
    count(release_quarter, name = "new_variables")
  print(counts)
  message(
    "Nuevo calendario guardado en: ",
    file.path(repo_root, "output_tidymodels", "calendar", "new_calendar.csv")
  )
  message(
    "Tabla LaTeX guardada en: ",
    file.path(
      repo_root,
      "output_tidymodels",
      "calendar",
      "calendar_variables_xgboost_full.tex"
    )
  )
}
