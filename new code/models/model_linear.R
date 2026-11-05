make_linear_model <- function(design, config) {
  if (design == "lasso") {
    return(
      parsnip::linear_reg(penalty = tune::tune(), mixture = 1) |>
        parsnip::set_engine("glmnet")
    )
  }
  parsnip::linear_reg() |>
    parsnip::set_engine("lm")
}

