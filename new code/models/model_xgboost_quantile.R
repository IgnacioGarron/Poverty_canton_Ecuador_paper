make_xgboost_quantile_model <- function(design, config) {
  parsnip::boost_tree(
    trees = tune::tune(),
    tree_depth = tune::tune(),
    min_n = tune::tune(),
    learn_rate = tune::tune(),
    loss_reduction = tune::tune(),
    sample_size = tune::tune(),
    mtry = tune::tune()
  ) |>
    parsnip::set_engine(
      "xgboost",
      counts = FALSE,
      nthread = config$xgb_threads,
      alpha = tune::tune("xgb_alpha"),
      lambda = tune::tune("xgb_lambda")
    ) |>
    parsnip::set_mode("quantile regression", quantile_levels = 0.50)
}
