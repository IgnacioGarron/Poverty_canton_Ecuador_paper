scenario_catalog <- function() {
  tibble::tribble(
    ~scenario_id,              ~processing_file,                              ~mobile_only, ~needs_figure_dir,
    "baseline",                "new code/data_processing_2022.R",            FALSE,        FALSE,
    "baseline_mobile_sum",     "new code/data_processing_2022_remote_sum.R", FALSE,        FALSE,
    "baseline_mobile_mean",    "new code/data_processing_2022_remote_mean.R",FALSE,        FALSE,
    "baseline_mobile_pca_sum", "new code/data_processing_2022_remote_pca_sum.R",FALSE,     FALSE,
    "baseline_mobile_pca_mean","new code/data_processing_2022_remote_pca_mean.R",FALSE,    FALSE,
    "mobile_only_sum",         "new code/data_processing_2022_remote_sum.R", TRUE,         FALSE,
    "mobile_only_mean",        "new code/data_processing_2022_remote_mean.R",TRUE,         FALSE
  )
}

scenario_spec <- function(scenario_id) {
  catalog <- scenario_catalog()
  spec <- dplyr::filter(catalog, .data$scenario_id == !!scenario_id)
  if (nrow(spec) != 1L) stop("Escenario desconocido: ", scenario_id)
  as.list(spec[1, ])
}

load_processing_output <- function(config, spec) {
  processing_path <- file.path(config$repo_root, spec$processing_file)
  if (!file.exists(processing_path)) stop("No existe: ", processing_path)
  processing_dir <- dirname(processing_path)

  processing_env <- new.env(parent = globalenv())
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(processing_dir)
  sys.source(processing_path, envir = processing_env)

  processing_function <- processing_env$preparar_data_consolidada
  args <- list()
  if ("transformar" %in% names(formals(processing_function))) {
    args$transformar <- FALSE
  }
  if (isTRUE(spec$needs_figure_dir)) {
    figure_dir <- file.path(config$output_root, "processing_figures")
    dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
    args$save_fig_dir <- paste0(figure_dir, .Platform$file.sep)
  }
  result <- do.call(processing_function, args)

  if (length(result$categoria_pri) != ncol(result$X1)) {
    stop("categoria_pri y X1 tienen distinto número de columnas en ", spec$scenario_id, ".")
  }
  mobile_columns <- result$categoria_pri == "Datos móbiles"
  mobile_columns[is.na(mobile_columns)] <- FALSE
  administrative_count <- sum(!mobile_columns)
  expected_mobile_count <- if (spec$scenario_id == "baseline") 0L else 38L

  if (!isTRUE(spec$mobile_only) && administrative_count != 947L) {
    stop(
      "El escenario ", spec$scenario_id, " debe contener 947 covariables administrativas; ",
      "se encontraron ", administrative_count, "."
    )
  }
  if (sum(mobile_columns) != expected_mobile_count) {
    stop(
      "El escenario ", spec$scenario_id, " debe contener ", expected_mobile_count,
      " covariables móviles crudas; se encontraron ", sum(mobile_columns), "."
    )
  }

  if (isTRUE(spec$mobile_only)) {
    if (sum(mobile_columns) != 38L) {
      stop(
        "El escenario mobile-only esperaba 38 covariables de movilidad; se encontraron ",
        sum(mobile_columns), "."
      )
    }
    result$X1 <- result$X1[, mobile_columns, drop = FALSE]
    result$X2 <- result$X2[, mobile_columns, drop = FALSE]
    result$categoria_pri <- result$categoria_pri[mobile_columns]
    result$categoria_sec <- result$categoria_sec[mobile_columns]
  }
  result
}

standardize_canton_code <- function(x) {
  x <- trimws(as.character(x))
  numeric_code <- grepl("^[0-9]+$", x)
  x[numeric_code] <- sprintf("%04d", as.integer(x[numeric_code]))
  x
}

excluded_analysis_canton_codes <- function() {
  c("2001", "2002", "2003")
}

build_model_frames <- function(processed, output_dir) {
  x1 <- as.data.frame(processed$X1, check.names = FALSE)
  x2 <- as.data.frame(processed$X2, check.names = FALSE)
  if (nrow(x1) != length(processed$Y_2022_censo)) {
    stop("X1 y Y_2022_censo tienen distinto número de filas.")
  }
  if (nrow(x2) != nrow(x1)) stop("X1 y X2 tienen distinto número de filas.")
  if (ncol(x1) != ncol(x2)) stop("X1 y X2 tienen distinto número de columnas.")

  original_names <- names(x1)
  safe_names <- make.names(original_names, unique = TRUE)
  names(x1) <- safe_names
  names(x2) <- safe_names
  variable_map <- tibble::tibble(
    position = seq_along(safe_names),
    original_name = original_names,
    model_name = safe_names
  )
  readr::write_csv(variable_map, file.path(output_dir, "variable_name_map.csv"))

  mobile_predictors <- processed$categoria_pri == "Datos móbiles"
  mobile_predictors[is.na(mobile_predictors)] <- FALSE

  is_numeric <- vapply(x1, is.numeric, logical(1)) & vapply(x2, is.numeric, logical(1))
  if (!all(is_numeric)) {
    stop("Hay covariables no numéricas: ", paste(safe_names[!is_numeric], collapse = ", "))
  }

  code <- standardize_canton_code(processed$CodCanton)
  canton <- as.character(processed$Canton)
  y <- as.numeric(processed$Y_2022_censo)
  y_2023_mef <- as.numeric(processed$Y_2023_mef)
  y_2023_enemdu <- as.numeric(processed$Y_2023_enemdu)
  if (anyDuplicated(code)) stop("CodCanton no es único en el output de procesamiento.")
  if (anyNA(y)) stop("Y_2022_censo contiene valores perdidos.")
  if (length(y_2023_mef) != nrow(x1) || length(y_2023_enemdu) != nrow(x1)) {
    stop("Las referencias de 2023 y las covariables tienen distinto número de filas.")
  }

  excluded_codes <- excluded_analysis_canton_codes()
  missing_excluded <- setdiff(excluded_codes, code)
  if (length(missing_excluded)) {
    stop(
      "No se encontraron los cantones de Galápagos que deben excluirse: ",
      paste(missing_excluded, collapse = ", ")
    )
  }
  excluded <- code %in% excluded_codes
  readr::write_csv(
    tibble::tibble(
      CodCanton = code[excluded],
      Canton = canton[excluded],
      reason = "Galápagos excluido del análisis para evitar sesgo geográfico"
    ),
    file.path(output_dir, "excluded_cantons.csv")
  )

  x1 <- x1[!excluded, , drop = FALSE]
  x2 <- x2[!excluded, , drop = FALSE]
  code <- code[!excluded]
  canton <- canton[!excluded]
  y <- y[!excluded]
  y_2023_mef <- y_2023_mef[!excluded]
  y_2023_enemdu <- y_2023_enemdu[!excluded]

  df_2022 <- tibble::as_tibble(x1) |>
    dplyr::mutate(
      CodCanton = code,
      Canton = canton,
      Y = y,
      poverty_group = poverty_group(y),
      .before = 1L
    )
  df_2023 <- tibble::as_tibble(x2) |>
    dplyr::mutate(
      CodCanton = code,
      Canton = canton,
      poverty_group = poverty_group(y),
      .before = 1L
    )

  list(
    df_2022 = df_2022,
    df_2023 = df_2023,
    predictor_names = safe_names,
    mobile_predictor_names = safe_names[mobile_predictors],
    y_2023_mef = y_2023_mef,
    y_2023_enemdu = y_2023_enemdu
  )
}

find_shapefile <- function(repo_root) {
  candidates <- c(
    file.path(repo_root, "input", "Data", "Mapa", "Cantones_ArcGIS_2012",
              "cantones_ecuador_2012.gpkg"),
    file.path(repo_root, "input", "Data", "Mapa", "Cantones",
              "ORGANIZACION TERRITORIAL DEL ESTADO CANTONAL.shp"),
    file.path(repo_root, "input", "Data", "Mapa", "cantones",
              "ORGANIZACION TERRITORIAL DEL ESTADO CANTONAL.shp"),
    file.path(repo_root, "Data", "Mapa", "cantones",
              "ORGANIZACION TERRITORIAL DEL ESTADO CANTONAL.shp")
  )
  found <- candidates[file.exists(candidates)]
  if (!length(found)) {
    stop(
      "No se encontró la capa de cantones. Ejecute: ",
      "Rscript 'download_Cantones_ArcGIS_2012.R'"
    )
  }
  found[[1]]
}

join_canton_geometry <- function(df_pred, repo_root, output_dir) {
  shapefile <- find_shapefile(repo_root)
  ecu_cantones <- sf::st_read(shapefile, quiet = TRUE) |>
    sf::st_make_valid() |>
    dplyr::mutate(DPA_CANTON = standardize_canton_code(.data$DPA_CANTON)) |>
    dplyr::filter(!(.data$DPA_CANTON %in% excluded_analysis_canton_codes()))

  # Unión solicitada: la geometría queda a la izquierda y los datos a la derecha.
  ecu_mapa <- ecu_cantones |>
    dplyr::left_join(df_pred, by = c("DPA_CANTON" = "CodCanton"))

  unmatched_shape <- ecu_mapa |>
    sf::st_drop_geometry() |>
    dplyr::filter(is.na(.data$Y)) |>
    dplyr::select(dplyr::any_of(c("DPA_CANTON", "DPA_DESCAN", "DPA_PROVIN", "DPA_DESPRO")))
  readr::write_csv(unmatched_shape, file.path(output_dir, "shapefile_cantons_without_model_data.csv"))

  data_without_shape <- dplyr::anti_join(
    dplyr::distinct(df_pred, .data$CodCanton),
    sf::st_drop_geometry(ecu_cantones) |>
      dplyr::transmute(CodCanton = .data$DPA_CANTON),
    by = "CodCanton"
  )
  readr::write_csv(data_without_shape, file.path(output_dir, "model_cantons_without_shape.csv"))
  if (nrow(data_without_shape)) {
    warning(
      nrow(data_without_shape),
      " cantones no tienen geometría en este shapefile y se excluyen solo de la validación espacial."
    )
  }

  matched <- ecu_mapa |>
    dplyr::filter(!is.na(.data$Y)) |>
    dplyr::mutate(CodCanton = .data$DPA_CANTON) |>
    dplyr::select(
      "CodCanton", "Canton", "poverty_group", "Y",
      dplyr::all_of(setdiff(names(df_pred), c("CodCanton", "Canton", "poverty_group", "Y")))
    )
  # La distancia entre polígonos usa bordes (muchos cantones contiguos quedan
  # a distancia cero). Un punto interior representativo produce clusters por
  # localización y reduce drásticamente el costo de la matriz de distancias.
  suppressWarnings(sf::st_point_on_surface(matched))
}
