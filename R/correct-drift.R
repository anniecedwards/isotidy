# Build a linear drift model from one observation per calibration point.

build_drift_model <- function(
    calibration_data,
    response_column,
    predictor_column,
    min_observations = 3L
) {
  if (!is.data.frame(calibration_data)) {
    stop("'calibration_data' must be a data frame.")
  }
  if (
    length(response_column) != 1L || is.na(response_column) ||
    !nzchar(response_column) || length(predictor_column) != 1L ||
    is.na(predictor_column) || !nzchar(predictor_column)
  ) {
    stop("'response_column' and 'predictor_column' must be column names.")
  }

  required_columns <- c(response_column, predictor_column)
  missing_columns <- setdiff(required_columns, names(calibration_data))
  if (length(missing_columns)) {
    stop(
      "Drift calibration data are missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  if (
    !is.numeric(calibration_data[[response_column]]) ||
    !is.numeric(calibration_data[[predictor_column]])
  ) {
    stop("Drift response and predictor columns must be numeric.")
  }
  if (
    !is.numeric(min_observations) || length(min_observations) != 1L ||
    !is.finite(min_observations) ||
    min_observations < 2L || min_observations != floor(min_observations)
  ) {
    stop("'min_observations' must be an integer of at least 2.")
  }

  valid <- is.finite(calibration_data[[response_column]]) &
    is.finite(calibration_data[[predictor_column]])
  model_data <- calibration_data[valid, , drop = FALSE]
  predictor_values <- model_data[[predictor_column]]
  predictor_range <- if (length(predictor_values)) {
    range(predictor_values)
  } else {
    c(NA_real_, NA_real_)
  }

  if (
    nrow(model_data) < min_observations ||
    length(unique(predictor_values)) < min_observations
  ) {
    return(list(
      model = NULL,
      calibration_data = model_data,
      n_observations = nrow(model_data),
      predictor_range = predictor_range
    ))
  }

  regression <- .fit_linear_regression(
    data = model_data,
    response = response_column,
    predictor = predictor_column
  )
  fitted_values <- stats::fitted(regression$model)
  reference_value <- mean(fitted_values)
  model_data$fitted_value <- fitted_values
  model_data$drift_offset <- fitted_values - reference_value
  model_data$corrected_value <-
    model_data[[response_column]] - model_data$drift_offset
  model_data$residual <- stats::residuals(regression$model)

  c(
    regression,
    list(
      calibration_data = model_data,
      reference_value = reference_value,
      n_observations = nrow(model_data),
      predictor_range = predictor_range
    )
  )
}
