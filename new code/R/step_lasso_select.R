step_lasso_select <- function(recipe, ..., outcome, penalty = tune::tune(),
                              min_selected = 1L,
                              role = NA, trained = FALSE, selected = NULL,
                              skip = FALSE, id = recipes::rand_id("lasso_select")) {
  recipes::add_step(
    recipe,
    step_lasso_select_new(
      terms = rlang::enquos(...),
      outcome = outcome,
      penalty = penalty,
      min_selected = min_selected,
      role = role,
      trained = trained,
      selected = selected,
      skip = skip,
      id = id
    )
  )
}

step_lasso_select_new <- function(terms, outcome, penalty, min_selected, role, trained,
                                  selected, skip, id) {
  recipes::step(
    subclass = "lasso_select",
    terms = terms,
    outcome = outcome,
    penalty = penalty,
    min_selected = as.integer(min_selected),
    role = role,
    trained = trained,
    selected = selected,
    removals = NULL,
    skip = skip,
    id = id
  )
}

prep.step_lasso_select <- function(x, training, info = NULL, ...) {
  predictor_names <- recipes::recipes_eval_select(x$terms, training, info)
  if (!x$outcome %in% names(training)) stop("No existe outcome: ", x$outcome)
  if (!is.numeric(x$penalty) || length(x$penalty) != 1L) {
    stop("penalty debe quedar finalizado antes de preparar step_lasso_select().")
  }

  x_matrix <- as.matrix(training[, predictor_names, drop = FALSE])
  fit <- glmnet::glmnet(
    x = x_matrix,
    y = training[[x$outcome]],
    family = "gaussian",
    alpha = 1,
    lambda = x$penalty,
    standardize = FALSE
  )
  coefficients <- as.matrix(stats::coef(fit, s = x$penalty))[-1, 1]
  selected <- names(coefficients)[abs(coefficients) > sqrt(.Machine$double.eps)]

  # glmnet puede dejar muy pocas variables para el PCA posterior. Se completa
  # con las covariables de mayor correlación únicamente hasta el mínimo pedido.
  target_n <- min(x$min_selected, length(predictor_names))
  if (length(selected) < target_n) {
    correlations <- vapply(training[, predictor_names, drop = FALSE], function(z) {
      value <- abs(stats::cor(z, training[[x$outcome]], use = "pairwise.complete.obs"))
      ifelse(is.finite(value), value, -Inf)
    }, numeric(1))
    additions <- names(sort(correlations, decreasing = TRUE))
    selected <- unique(c(selected, additions))[seq_len(target_n)]
  }

  out <- step_lasso_select_new(
    terms = x$terms,
    outcome = x$outcome,
    penalty = x$penalty,
    min_selected = x$min_selected,
    role = x$role,
    trained = TRUE,
    selected = selected,
    skip = x$skip,
    id = x$id
  )
  out$removals <- setdiff(predictor_names, selected)
  out
}

bake.step_lasso_select <- function(object, new_data, ...) {
  recipes::recipes_remove_cols(new_data, object)
}

print.step_lasso_select <- function(x, width = max(20, options()$width - 30), ...) {
  title <- if (x$trained) "LASSO retained " else "LASSO selection on "
  items <- if (x$trained) x$selected else x$terms
  recipes::print_step(items, x$terms, x$trained, title, width)
  invisible(x)
}

tunable.step_lasso_select <- function(x, ...) {
  tibble::tibble(
    name = "penalty",
    call_info = list(list(pkg = "dials", fun = "penalty", range = c(-5, 0))),
    source = "recipe",
    component = "step_lasso_select",
    component_id = x$id
  )
}

required_pkgs.step_lasso_select <- function(x, ...) "glmnet"
