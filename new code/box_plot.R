#!/usr/bin/env Rscript

suppressWarnings({
  suppressPackageStartupMessages({
    library(dplyr)
    library(ggplot2)
    library(readr)
  })
})

scenarios <- c("baseline", "baseline_mobile_pca_mean","baseline_mobile_mean","mobile_only_mean")
output_root <- file.path("..", "output_tidymodels")
figure_dir <- file.path(output_root, "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

model_levels <- c("linear", "xgboost", "xgboost_quantile")
design_levels <- c("full", "lasso", "pcr", "pcr_lasso", "pls")

model_palette <- c(
  linear = "#0072B2",
  xgboost = "#D55E00",
  xgboost_quantile = "#009E73"
)

model_labels <- c(
  linear = "Linear",
  xgboost = "XGBoost",
  xgboost_quantile = "XGBoost quantile"
)

design_labels <- c(
  full = "Full",
  lasso = "LASSO",
  pcr = "PCR",
  pcr_lasso = "PCR + LASSO",
  pls = "PLS"
)

plot_oof_metric <- function(data, metric, y_label, zero_line = FALSE) {
  plot <- ggplot(
    data,
    aes(
      x = design,
      y = .data[[metric]],
      fill = model,
      group = interaction(design, model)
    )
  )

  if (zero_line) {
    plot <- plot + geom_hline(
      yintercept = 0,
      linetype = "dashed",
      linewidth = 0.45,
      color = "grey35"
    )
  }

  plot +
    geom_boxplot(
      position = position_dodge(width = 0.78),
      width = 0.66,
      alpha = 0.48,
      linewidth = 0.5,
      outlier.shape = NA
    ) +
    geom_point(
      position = position_jitterdodge(
        jitter.width = 0.07,
        dodge.width = 0.78,
        seed = 2026
      ),
      shape = 21,
      size = 2.3,
      stroke = 0.35,
      color = "white"
    ) +
    scale_fill_manual(
      values = model_palette,
      breaks = model_levels,
      labels = model_labels,
      drop = FALSE
    ) +
    scale_x_discrete(
      breaks = design_levels,
      labels = design_labels,
      drop = FALSE
    ) +
    labs(
      x = "Covariate design",
      y = y_label,
      fill = "Model"
    ) +
    theme_bw(base_size = 12) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.margin = margin(10, 14, 10, 10)
    ) 
}

for (scenario in scenarios) {
  prediction_dir <- file.path(output_root, scenario, "predictions")
  oof_files <- list.files(
    prediction_dir,
    pattern = "^oof__.+__.+[.]csv$",
    full.names = TRUE
  )

  oof_predictions <- bind_rows(lapply(oof_files, function(file) {
    file_parts <- strsplit(
      tools::file_path_sans_ext(basename(file)),
      "__",
      fixed = TRUE
    )[[1]]

    read_csv(file, show_col_types = FALSE, progress = FALSE) |>
      transmute(
        model = file_parts[2],
        design = file_parts[3],
        fold,
        truth,
        estimate
      )
  }))

  fold_metrics <- oof_predictions |>
    mutate(error_pp = 100 * (estimate - truth)) |>
    group_by(model, design, fold) |>
    summarise(
      rmse_oof_pp = sqrt(mean(error_pp^2)),
      bias_oof_pp = mean(error_pp),
      .groups = "drop"
    ) |>
    mutate(
      model = factor(model, levels = model_levels),
      design = factor(design, levels = design_levels)
    )

  # Guardar los datos fuente de cada figura con el mismo nombre base del PNG.
  # Se conserva una fila por modelo, diseño y fold espacial.
  write_csv(
    fold_metrics |>
      transmute(
        scenario = scenario,
        model = as.character(model),
        design = as.character(design),
        fold,
        rmse_oof_pp
      ),
    file.path(figure_dir, paste0("rmse_oof_", scenario, "_boxplot.csv"))
  )

  write_csv(
    fold_metrics |>
      transmute(
        scenario = scenario,
        model = as.character(model),
        design = as.character(design),
        fold,
        bias_oof_pp
      ),
    file.path(figure_dir, paste0("bias_oof_", scenario, "_boxplot.csv"))
  )

  plot_rmse <- plot_oof_metric(
    fold_metrics,
    metric = "rmse_oof_pp",
    y_label = "OOS RMSE (pp)"
  )
  
  plot_rmse <- plot_rmse + ylim(c(0,25)) + geom_hline(yintercept = 0)

  plot_bias <- plot_oof_metric(
    fold_metrics,
    metric = "bias_oof_pp",
    y_label = "OOS Bias (pp)",
    zero_line = TRUE
  )
  
  plot_bias <- plot_bias + ylim(c(-10,10))

  ggsave(
    file.path(figure_dir, paste0("rmse_oof_", scenario, "_boxplot.png")),
    plot_rmse,
    width = 5,
    height = 3,
    dpi = 300,
    bg = "white"
  )

  ggsave(
    file.path(figure_dir, paste0("bias_oof_", scenario, "_boxplot.png")),
    plot_bias,
    width = 5,
    height = 3,
    dpi = 300,
    bg = "white"
  )
}
