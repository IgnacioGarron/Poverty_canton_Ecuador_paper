base_recipe <- function(training_data, predictor_names) {
  recipes::recipe(Y ~ ., data = training_data[, c("Y", "CodCanton", "Canton", "poverty_group", predictor_names)]) |>
    recipes::update_role(CodCanton, Canton, poverty_group, new_role = "id") |>
    recipes::step_zv(recipes::all_numeric_predictors()) |>
    recipes::step_normalize(recipes::all_numeric_predictors())
}

add_scenario_preprocessing <- function(recipe, scenario_id, mobile_predictor_names) {
  pca_scenarios <- c("baseline_mobile_pca_sum", "baseline_mobile_pca_mean")
  if (!scenario_id %in% pca_scenarios) return(recipe)
  if (length(mobile_predictor_names) != 38L) {
    stop(
      "El escenario ", scenario_id,
      " requiere 38 variables móviles crudas para estimar su PCA dentro del fold."
    )
  }

  recipe |>
    recipes::step_pca(
      tidyselect::any_of(mobile_predictor_names),
      num_comp = 1L,
      prefix = "mobility_PC"
    )
}

add_covariate_design <- function(recipe, design, model_name) {
  if (design == "full") return(recipe)

  if (design == "pcr") {
    return(recipe |>
      recipes::step_pca(recipes::all_numeric_predictors(), num_comp = tune::tune()))
  }

  if (design == "pls") {
    return(recipe |>
      recipes::step_pls(
        recipes::all_numeric_predictors(),
        outcome = "Y",
        num_comp = tune::tune(),
        predictor_prop = 1
      ))
  }

  if (design == "lasso") {
    # En el modelo lineal, glmnet hace simultáneamente selección y estimación.
    if (model_name == "linear") return(recipe)
    return(recipe |>
      step_lasso_select(
        recipes::all_numeric_predictors(), outcome = "Y", penalty = tune::tune()
      ))
  }

  if (design == "pcr_lasso") {
    return(recipe |>
      step_lasso_select(
        recipes::all_numeric_predictors(), outcome = "Y", penalty = tune::tune(),
        min_selected = 10L
      ) |>
      recipes::step_pca(recipes::all_numeric_predictors(), num_comp = tune::tune()))
  }

  stop("Diseño desconocido: ", design)
}
