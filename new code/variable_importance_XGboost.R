#!/usr/bin/env Rscript

suppressWarnings(suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(readxl)
}))

scenarios <- c(
  "baseline",
  "baseline_mobile_pca_mean",
  "baseline_mobile_mean",
  "mobile_only_mean"
)
scenario_labels <- c(
  baseline = "Baseline",
  baseline_mobile_pca_mean = "Baseline + mobility PCA (mean)",
  baseline_mobile_mean = "Baseline + mobility (mean)",
  mobile_only_mean = "Mobility only (mean)"
)
output_root <- file.path("..", "output_tidymodels")
figure_dir <- file.path(output_root, "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

english_map_file <- file.path(
  "..",
  "input",
  "Data",
  "Covariables_Actualizadas_2010_enviar",
  "top_var_english.xlsx"
)
if (!file.exists(english_map_file)) {
  stop("No existe el diccionario de nombres en inglés: ", english_map_file)
}

english_name_map <- read_excel(
  english_map_file,
  sheet = "Variables"
) |>
  transmute(
    variable_original = as.character(`Original Variable Name`),
    variable_english = as.character(`English Name`)
  ) |>
  bind_rows(
    tibble::tribble(
      ~variable_original, ~variable_english,
      "ParticipacionVentasExplotación de minas y canteras",
      "Sales Share: Mining and Quarrying",
      "TasaPromocionFemeninoSegundoAnoBACH",
      "Promotion Rate, Second Year Baccalaureate (Female)",
      "CajerosAutomáticosTasa1000km",
      "ATMs per 1,000 km²",
      "Porcentajenacprematuromoderadomujertotalmadre",
      "Moderate Preterm Birth Rate (Female)",
      "ParticipacionPIBManufactura",
      "GDP Share: Manufacturing",
      "Mobility PC1",
      "Mobility Factor"
    )
  ) |>
  distinct(variable_original, .keep_all = TRUE)

model_name <- "xgboost"
design_name <- "full"
top_n <- 20L

wrap_labels <- function(x, width = 42L) {
  vapply(
    x,
    function(label) paste(strwrap(label, width = width), collapse = "\n"),
    character(1)
  )
}

translate_mobile_variable <- function(x) {
  base_labels <- c(
    viajes = "Trips",
    viajeros = "Travelers",
    monday = "Monday Mobility",
    tuesday = "Tuesday Mobility",
    wednesday = "Wednesday Mobility",
    thursday = "Thursday Mobility",
    friday = "Friday Mobility",
    saturday = "Saturday Mobility",
    sunday = "Sunday Mobility",
    weekday_all = "Weekday Mobility",
    weekday_morning = "Weekday Morning Mobility",
    weekday_afternoon = "Weekday Afternoon Mobility",
    weekday_evening = "Weekday Evening Mobility",
    weekday_night = "Weekday Night Mobility",
    weekend_all = "Weekend Mobility",
    weekend_morning = "Weekend Morning Mobility",
    weekend_afternoon = "Weekend Afternoon Mobility",
    weekend_evening = "Weekend Evening Mobility",
    weekend_night = "Weekend Night Mobility"
  )
  network_labels <- c(
    x = "Internal",
    y = "National"
  )

  is_mobile <- grepl("[.][xy]$", x)
  base_name <- sub("[.][xy]$", "", x)
  suffix <- sub("^.*[.]", "", x)
  base_label <- unname(base_labels[base_name])
  network_label <- unname(network_labels[suffix])
  translated <- ifelse(
    is_mobile & !is.na(base_label) & !is.na(network_label),
    paste0(base_label, " (", network_label, ")"),
    NA_character_
  )
  translated
}

for (scenario in scenarios) {
  model_file <- file.path(
    output_root,
    scenario,
    "models",
    paste0(model_name, "__", design_name, ".rds")
  )
  variable_map_file <- file.path(
    output_root,
    scenario,
    "variable_name_map.csv"
  )

  if (!file.exists(model_file)) {
    stop("No existe el modelo final para el escenario ", scenario, ": ", model_file)
  }
  if (!file.exists(variable_map_file)) {
    stop("No existe el mapa de nombres para el escenario ", scenario, ": ", variable_map_file)
  }

  fitted_workflow <- readRDS(model_file)
  xgb_fit <- workflows::extract_fit_engine(fitted_workflow)
  prepared_predictors <- workflows::extract_mold(fitted_workflow)$predictors
  variable_map <- read_csv(
    variable_map_file,
    show_col_types = FALSE,
    progress = FALSE
  )

  # TreeSHAP exacto sobre los 218 cantones usados para ajustar el workflow
  # final. La última columna es el valor base y no es una covariable.
  predictor_matrix <- as.matrix(prepared_predictors)
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
    stop("No se pudo identificar una única columna de intercepto SHAP en ", scenario)
  }
  if (ncol(shap_values) != ncol(predictor_matrix) + 1L) {
    stop("El número de columnas SHAP no coincide con los predictores en ", scenario)
  }

  model_prediction <- predict(
    xgb_fit,
    newdata = xgboost::xgb.DMatrix(predictor_matrix)
  )
  additivity_error <- max(abs(rowSums(shap_values) - model_prediction))
  if (!is.finite(additivity_error) || additivity_error > 1e-4) {
    stop("Falló la comprobación de aditividad SHAP en ", scenario)
  }

  shap_features <- shap_values[, -intercept_column, drop = FALSE]
  shap_feature_names <- feature_names[-intercept_column]

  importance <- tibble::tibble(
    Feature = shap_feature_names,
    mean_abs_shap = colMeans(abs(shap_features)),
    mean_shap = colMeans(shap_features),
    n_cantons = nrow(shap_features)
  ) |>
    left_join(
      variable_map |>
        select(model_name, original_name),
      by = c("Feature" = "model_name")
    ) |>
    mutate(
      variable = coalesce(original_name, Feature),
      variable = if_else(
        variable == "mobility_PC1",
        "Mobility PC1",
        variable
      )
    ) |>
    arrange(desc(mean_abs_shap), Feature) |>
    slice_head(n = top_n) |>
    left_join(english_name_map, by = c("variable" = "variable_original")) |>
    mutate(
      rank = row_number(),
      variable_english = coalesce(
        variable_english,
        translate_mobile_variable(variable)
      ),
      # Evita niveles duplicados si dos columnas comparten el nombre original.
      plot_label = make.unique(variable_english),
      plot_label = factor(plot_label, levels = rev(plot_label))
    )

  untranslated <- importance |>
    filter(is.na(variable_english) | !nzchar(variable_english)) |>
    pull(variable)
  if (length(untranslated)) {
    stop(
      "Faltan nombres en inglés para ", scenario, ": ",
      paste(untranslated, collapse = ", ")
    )
  }

  figure_stub <- paste0(
    "variable_importance_", model_name, "_", design_name, "_", scenario
  )

  write_csv(
    importance |>
      transmute(
        scenario = scenario,
        model = model_name,
        design = design_name,
        rank,
        variable_original = variable,
        variable = variable_english,
        model_feature = Feature,
        mean_abs_shap,
        mean_abs_shap_pp = 100 * mean_abs_shap,
        mean_shap,
        mean_shap_pp = 100 * mean_shap,
        n_cantons,
        max_additivity_error = additivity_error
      ),
    file.path(figure_dir, paste0(figure_stub, ".csv"))
  )

  plot <- ggplot(
    importance,
    aes(x = plot_label, y = 100 * mean_abs_shap)
  ) +
    geom_col(width = 0.72, fill = "gray") +
    coord_flip() +
    scale_x_discrete(labels = wrap_labels) +
    scale_y_continuous(
      labels = scales::label_number(accuracy = 0.1),
      expand = expansion(mult = c(0, 0.04))
    ) +
    labs(
      x = NULL,
      y = "Mean |SHAP| (pp)",
      title = "",
      subtitle = paste0("")
    ) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.title.position = "plot",
      plot.title = element_text(face = "bold"),
      plot.margin = margin(10, 14, 10, 10)
    )

  ggsave(
    file.path(figure_dir, paste0(figure_stub, ".png")),
    plot,
    width = 7,
    height = 6,
    dpi = 300,
    bg = "white"
  )
}
