new_code_root <- normalizePath(getwd(), mustWork = TRUE)
repo_root <- normalizePath(file.path(new_code_root, ".."), mustWork = TRUE)
source(file.path(new_code_root, "R", "load_all.R"))
source_new_pipeline(repo_root)

required <- c("tidymodels", "spatialsample", "sf", "readxl", "readr", "haven")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop(
    "Faltan paquetes: ", paste(missing, collapse = ", "),
    ". Ejecute: Rscript 'setup_dependencies.R'"
  )
}
attach_processing_packages()

scenario_id <- Sys.getenv("CV_SCENARIO", unset = "baseline")
config <- pipeline_config(repo_root, scenario_id)
validate_config(config)
figure_dir <- file.path(repo_root, "output_tidymodels", "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# Los folds dependen solamente del código y la localización del cantón. Se usa
# baseline por defecto para recuperar los códigos, sin cargar covariables.
processed <- load_processing_output(config, scenario_spec(scenario_id))
df_pred <- tibble::tibble(
  CodCanton = standardize_canton_code(processed$CodCanton),
  Canton = as.character(processed$Canton),
  Y = as.numeric(processed$Y_2022_censo)
) |>
  dplyr::filter(!(.data$CodCanton %in% excluded_analysis_canton_codes()))

canton_layer <- find_shapefile(repo_root)
ecu_cantones <- sf::st_read(canton_layer, quiet = TRUE) |>
  sf::st_make_valid() |>
  dplyr::mutate(DPA_CANTON = standardize_canton_code(.data$DPA_CANTON)) |>
  dplyr::filter(!(.data$DPA_CANTON %in% excluded_analysis_canton_codes()))

model_codes <- sort(unique(df_pred$CodCanton))
shape_codes <- sort(unique(ecu_cantones$DPA_CANTON))
missing_in_shape <- setdiff(model_codes, shape_codes)
extra_in_shape <- setdiff(shape_codes, model_codes)

if (
  nrow(df_pred) != 218L ||
  length(model_codes) != 218L ||
  nrow(ecu_cantones) != 218L ||
  length(shape_codes) != 218L ||
  length(missing_in_shape) > 0L ||
  length(extra_in_shape) > 0L
) {
  stop(
    "cv_map.R requiere una correspondencia exacta de 218 cantones continentales. ",
    "Faltan en la capa: ",
    if (length(missing_in_shape)) paste(missing_in_shape, collapse = ", ") else "ninguno",
    "; sobran en la capa: ",
    if (length(extra_in_shape)) paste(extra_in_shape, collapse = ", ") else "ninguno",
    ". Ejecute primero: Rscript 'download_Cantones_ArcGIS_2012.R'"
  )
}

# Misma unión utilizada por el pipeline de estimación.
ecu_mapa <- ecu_cantones |>
  dplyr::left_join(df_pred, by = c("DPA_CANTON" = "CodCanton"))

matched_polygons <- ecu_mapa |>
  dplyr::filter(!is.na(.data$Y)) |>
  dplyr::mutate(CodCanton = .data$DPA_CANTON) |>
  dplyr::select("CodCanton", "Canton", "Y")

spatial_points <- suppressWarnings(sf::st_point_on_surface(matched_polygons))
folds <- make_spatial_folds(spatial_points, config$outer_v, config)
membership <- fold_membership(folds) |>
  dplyr::mutate(
    outer_fold = factor(
      .data$outer_fold,
      levels = paste0("Fold", seq_len(config$outer_v)),
      labels = paste("Fold", seq_len(config$outer_v))
    )
  )

map_folds <- ecu_mapa |>
  dplyr::left_join(membership, by = c("DPA_CANTON" = "CodCanton"))

fold_counts <- membership |>
  dplyr::count(.data$outer_fold, name = "n_cantons")
readr::write_csv(
  fold_counts,
  file.path(figure_dir, paste0("spatial_cv_fold_counts_v", config$outer_v, ".csv"))
)

plot_folds <- ggplot2::ggplot(map_folds) +
  ggplot2::geom_sf(
    ggplot2::aes(fill = .data$outer_fold),
    color = "white",
    linewidth = 0.12
  ) +
  ggplot2::scale_fill_viridis_d(
    option = "D",
    end = 0.90,
    na.value = "grey88",
    name = "Spatial fold",
    drop = FALSE
  ) +
  ggplot2::coord_sf(datum = NA) +
  ggplot2::labs(
    title = "",
    subtitle = paste0(""),
    caption = paste0("")
  ) +
  ggplot2::theme_void(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 16, hjust = 0),
    plot.subtitle = ggplot2::element_text(size = 11, color = "grey30"),
    plot.caption = ggplot2::element_text(size = 9, color = "grey35", hjust = 0),
    legend.position = "bottom",
    legend.title = ggplot2::element_text(face = "bold"),
    legend.key.width = grid::unit(1.5, "lines"),
    plot.margin = ggplot2::margin(12, 16, 12, 16)
  )

figure_path <- file.path(
  figure_dir,
  paste0("spatial_cv_folds_v", config$outer_v, ".png")
)
ggplot2::ggsave(
  filename = figure_path,
  plot = plot_folds,
  width = 9,
  height = 8,
  dpi = 300,
  bg = "white"
)

message("Figura guardada en: ", figure_path)
