# Shared simple linear-regression fitting and summary extraction.

.fit_linear_regression <- function(data, response, predictor) {
  required_columns <- c(response, predictor)
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns)) {
    stop(
      "Regression data are missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  model <- stats::lm(
    stats::reformulate(predictor, response = response),
    data = data
  )
  model_summary <- summary(model)
  coefficients <- stats::coef(model)
  coefficient_summary <- model_summary$coefficients

  list(
    model = model,
    slope = unname(coefficients[[predictor]]),
    intercept = unname(coefficients[["(Intercept)"]]),
    r_squared = model_summary$r.squared,
    residual_std_error = model_summary$sigma,
    slope_p_value = unname(coefficient_summary[predictor, "Pr(>|t|)"])
  )
}
