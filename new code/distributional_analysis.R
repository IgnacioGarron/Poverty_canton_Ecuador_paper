#!/usr/bin/env Rscript

suppressWarnings(suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
}))

scenarios <- c("baseline", "baseline_mobile_pca_mean","baseline_mobile_mean","mobile_only_mean")
output_root <- file.path("..", "output_tidymodels")
figure_dir <- file.path(output_root, "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# INEC, Censo 2022: 01_2022_CPV_Estructura_poblacional, tabla 1.1.
# https://www.censoecuador.gob.ec/wp-content/uploads/2024/04/01_2022_CPV_Estructura_poblacional_CSV.zip
population <- read_csv(
  file.path("..", "input", "Data", "INEC", "population_canton_2022.csv"),
  show_col_types = FALSE
) |>
  select(CodCanton, population_2022)

designs <- c("full", "lasso", "pcr", "pcr_lasso", "pls")
design_labels <- c("Full", "LASSO", "PCR", "PCR + LASSO", "PLS")
design_palette <- c(
  full = "#000000",
  lasso = "#0072B2",
  pcr = "#D55E00",
  pcr_lasso = "#009E73",
  pls = "#CC79A7"
)
design_linetypes <- c(
  full = "solid",
  lasso = "dashed",
  pcr = "dotted",
  pcr_lasso = "dotdash",
  pls = "longdash"
)

for (scenario in scenarios) {
  files <- list.files(
    file.path(output_root, scenario, "predictions"),
    pattern = "^oof__xgboost__.+[.]csv$",
    full.names = TRUE
  )

  predictions <- bind_rows(lapply(files, function(file) {
    read_csv(file, show_col_types = FALSE, progress = FALSE) |>
      mutate(
        design = sub(
          "^oof__xgboost__",
          "",
          tools::file_path_sans_ext(basename(file))
        )
      )
  })) |>
    left_join(population, by = "CodCanton") |>
    mutate(
      design = factor(design, levels = designs),
      truth_pp = 100 * truth,
      error_pp = 100 * (estimate - truth)
    )

  # Datos cantonales que alimentan las curvas LOESS de la figura.
  write_csv(
    predictions |>
      transmute(
        scenario = scenario,
        model = "xgboost",
        design = as.character(design),
        CodCanton,
        Canton,
        fold,
        truth,
        estimate,
        truth_pp,
        error_pp,
        population_2022
      ),
    file.path(figure_dir, paste0("distributional_bias_", scenario, ".csv"))
  )

  plot <- ggplot(
    predictions,
    aes(
      truth_pp,
      error_pp,
      color = design,
      linetype = design,
      weight = population_2022
    )
  ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    geom_smooth(
      method = "loess",
      formula = y ~ x,
      span = 0.75,
      se = FALSE,
      linewidth = 0.75
    ) +
    scale_color_manual(
      values = design_palette,
      breaks = designs,
      labels = design_labels
    ) +
    scale_linetype_manual(
      values = design_linetypes,
      breaks = designs,
      labels = design_labels
    ) +
    scale_x_continuous(breaks = seq(0, 100, 20)) +
    scale_y_continuous(breaks = seq(-50, 50, 10)) +
    coord_cartesian(xlim = c(0, 100), ylim = c(-50, 50)) +
    labs(
      x = "True 2022 poverty rate (percentage points)",
      y = "OOS bias (pp)",
      color = "Covariate design",
      linetype = "Covariate design"
    ) +
    theme_bw(base_size = 12) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )

  ggsave(
    file.path(figure_dir, paste0("distributional_bias_", scenario, ".png")),
    plot,
    width = 5,
    height = 3,
    dpi = 300,
    bg = "white"
  )
}
