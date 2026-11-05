median_pinball_loss <- function(data, ...) {
  UseMethod("median_pinball_loss")
}

median_pinball_loss.data.frame <- function(data, truth, estimate,
                                            na_rm = TRUE, case_weights = NULL, ...) {
  yardstick::quantile_metric_summarizer(
    name = "median_pinball_loss",
    fn = median_pinball_loss_vec,
    data = data,
    truth = !!rlang::enquo(truth),
    estimate = !!rlang::enquo(estimate),
    na_rm = na_rm,
    case_weights = !!rlang::enquo(case_weights)
  )
}

median_pinball_loss_vec <- function(truth, estimate, na_rm = TRUE,
                                     case_weights = NULL, ...) {
  yardstick::check_quantile_metric(truth, estimate, case_weights)
  levels <- hardhat::extract_quantile_levels(estimate)
  median_position <- which(abs(levels - 0.5) < sqrt(.Machine$double.eps))
  if (length(median_position) != 1L) {
    stop("La métrica requiere exactamente una predicción para el cuantil 0.5.")
  }
  median_estimate <- as.matrix(estimate)[, median_position]
  residual <- truth - median_estimate
  loss <- ifelse(residual >= 0, 0.5 * residual, -0.5 * residual)

  if (na_rm) {
    cleaned <- yardstick::yardstick_remove_missing(truth, loss, case_weights)
    loss <- cleaned$estimate
    case_weights <- cleaned$case_weights
  } else if (yardstick::yardstick_any_missing(truth, loss, case_weights)) {
    return(NA_real_)
  }
  if (!length(loss)) return(NA_real_)
  if (is.null(case_weights)) mean(loss) else stats::weighted.mean(loss, as.numeric(case_weights))
}

median_pinball_loss <- yardstick::new_quantile_metric(
  median_pinball_loss,
  direction = "minimize"
)

poverty_group <- function(y) {
  cut(
    y,
    breaks = c(-Inf, 0.25, 0.50, 0.75, Inf),
    labels = c("Low", "Medium", "High", "Very_high"),
    right = TRUE
  )
}

overall_metrics <- function(predictions, evaluation) {
  truth <- predictions$truth
  estimate <- predictions$estimate
  error <- estimate - truth
  tibble::tibble(
    evaluation = evaluation,
    n = length(truth),
    rmse = sqrt(mean(error^2)),
    rmse_pp = 100 * sqrt(mean(error^2)),
    nrmse_sd = sqrt(mean(error^2)) / stats::sd(truth),
    mae = mean(abs(error)),
    mae_pp = 100 * mean(abs(error)),
    r_squared = if (stats::sd(estimate) == 0) NA_real_ else stats::cor(truth, estimate)^2,
    mean_signed_error = mean(error),
    mean_signed_error_pp = 100 * mean(error)
  )
}

group_error_metrics <- function(predictions, evaluation = "nested_spatial_oof") {
  errors <- predictions |>
    dplyr::mutate(
      poverty_group = poverty_group(.data$truth),
      error_pp = 100 * (.data$estimate - .data$truth)
    )

  point_metrics <- errors |>
    dplyr::group_by(.data$poverty_group, .drop = FALSE) |>
    dplyr::summarise(
      evaluation = evaluation,
      n = dplyr::n(),
      mean_signed_error_pp = ifelse(.data$n > 0, mean(.data$error_pp), NA_real_),
      mae_pp = ifelse(.data$n > 0, mean(abs(.data$error_pp)), NA_real_),
      rmse_pp = ifelse(.data$n > 0, sqrt(mean(.data$error_pp^2)), NA_real_),
      .groups = "drop"
    )

  fold_uncertainty <- errors |>
    dplyr::group_by(.data$poverty_group, .data$fold, .drop = FALSE) |>
    dplyr::summarise(
      fold_mean_signed_error_pp = ifelse(dplyr::n() > 0, mean(.data$error_pp), NA_real_),
      .groups = "drop"
    ) |>
    dplyr::filter(is.finite(.data$fold_mean_signed_error_pp)) |>
    dplyr::group_by(.data$poverty_group, .drop = FALSE) |>
    dplyr::summarise(
      n_folds_with_group = dplyr::n(),
      se_across_folds_pp = ifelse(
        .data$n_folds_with_group > 1,
        stats::sd(.data$fold_mean_signed_error_pp) / sqrt(.data$n_folds_with_group),
        NA_real_
      ),
      t_critical = ifelse(
        .data$n_folds_with_group > 1,
        stats::qt(0.975, df = .data$n_folds_with_group - 1),
        NA_real_
      ),
      .groups = "drop"
    )

  point_metrics |>
    dplyr::left_join(fold_uncertainty, by = "poverty_group") |>
    dplyr::mutate(
      ci95_low_pp = .data$mean_signed_error_pp - .data$t_critical * .data$se_across_folds_pp,
      ci95_high_pp = .data$mean_signed_error_pp + .data$t_critical * .data$se_across_folds_pp
    ) |>
    dplyr::select(-dplyr::all_of("t_critical"))
}

generalization_gap <- function(oof_metrics, in_sample_metrics) {
  tibble::tibble(
    rmse_in_sample = in_sample_metrics$rmse,
    rmse_nested_spatial_oof = oof_metrics$rmse,
    absolute_gap = oof_metrics$rmse - in_sample_metrics$rmse,
    ratio_oof_to_in_sample = oof_metrics$rmse / in_sample_metrics$rmse,
    note = "In-sample es diagnóstico; nested spatial OOF es la estimación de validación."
  )
}
