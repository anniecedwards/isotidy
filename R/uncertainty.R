# Monte Carlo helpers for shared calibration uncertainty ------------------

# Draw coefficient vectors jointly so fitted coefficient covariance is kept.
.draw_multivariate_normal <- function(n, mean, covariance) {
  if (!is.matrix(covariance) || nrow(covariance) != length(mean) ||
      ncol(covariance) != length(mean)) {
    stop("'covariance' must be a square matrix matching 'mean'.")
  }
  decomposition <- eigen(covariance, symmetric = TRUE)
  if (any(!is.finite(decomposition$values)) ||
      any(decomposition$values < -1e-10)) {
    stop("'covariance' must be finite and positive semidefinite.")
  }
  root <- decomposition$vectors %*%
    diag(sqrt(pmax(decomposition$values, 0)), nrow = length(mean))
  draws <- matrix(stats::rnorm(n * length(mean)), nrow = n) %*% t(root)
  draws <- sweep(draws, 2, mean, `+`)
  colnames(draws) <- names(mean)
  draws
}

#' Draw predictions from a fitted linear calibration model
#'
#' Draws model coefficients jointly from the fitted coefficient covariance and
#' predicts all supplied rows with each shared coefficient draw. The returned
#' matrix has one Monte Carlo draw per row and one prediction per new-data row.
#' These are calibration-parameter draws; measurement and model-discrepancy
#' terms must be added separately when appropriate.
#'
#' @param model A fitted `lm` calibration model.
#' @param newdata Data frame with predictors required by `model`.
#' @param nsim Number of Monte Carlo draws.
#' @return Numeric matrix of predictions, draws by new-data rows.
#' @export
draw_calibration_predictions <- function(model, newdata, nsim = 10000L) {
  if (!inherits(model, "lm")) stop("'model' must be a fitted lm object.")
  if (!is.data.frame(newdata)) stop("'newdata' must be a data frame.")
  if (length(nsim) != 1L || !is.finite(nsim) || nsim < 1 ||
      nsim != floor(nsim)) {
    stop("'nsim' must be a positive integer.")
  }

  coefficients <- stats::coef(model)
  if (anyNA(coefficients)) {
    stop("Cannot draw calibration predictions from aliased coefficients.")
  }
  if (length(model$offset)) {
    stop("Models with an offset are not supported by this helper.")
  }
  covariance <- stats::vcov(model)
  coefficient_draws <- .draw_multivariate_normal(
    as.integer(nsim), coefficients, covariance
  )
  model_terms <- stats::delete.response(stats::terms(model))
  design <- stats::model.matrix(
    model_terms,
    newdata,
    contrasts.arg = model$contrasts,
    xlev = model$xlevels
  )
  missing_coefficients <- setdiff(colnames(design), names(coefficients))
  if (length(missing_coefficients)) {
    stop("New data produce terms absent from the fitted model: ",
         paste(missing_coefficients, collapse = ", "))
  }
  design <- design[, names(coefficients), drop = FALSE]
  predictions <- coefficient_draws %*% t(design)
  colnames(predictions) <- rownames(newdata) %||% as.character(seq_len(nrow(newdata)))
  predictions
}

#' Calculate a Monte Carlo distribution for a plotted linear slope
#'
#' Fits a slope for every row of a matrix of simulated final values. Matrix
#' rows are draws and columns correspond to observations and entries of `x`.
#'
#' @param x Numeric predictor values used in the final plot.
#' @param y_draws Numeric matrix of simulated final values, draws by observations.
#' @param conf Confidence level for the equal-tail interval.
#' @return List with point estimate, standard uncertainty, interval, and all
#'   simulated slopes.
#' @export
summarize_slope_uncertainty <- function(x, y_draws, conf = 0.95) {
  if (!is.numeric(x) || length(x) < 3L || any(!is.finite(x))) {
    stop("'x' must contain at least three finite numeric values.")
  }
  if (!is.matrix(y_draws) || !is.numeric(y_draws) ||
      ncol(y_draws) != length(x) || nrow(y_draws) < 2L ||
      any(!is.finite(y_draws))) {
    stop("'y_draws' must be a finite numeric matrix with one column per x.")
  }
  if (length(conf) != 1L || !is.finite(conf) || conf <= 0 || conf >= 1) {
    stop("'conf' must be between 0 and 1.")
  }

  centered_x <- x - mean(x)
  slope_draws <- drop(y_draws %*% centered_x) / sum(centered_x^2)
  interval <- stats::quantile(
    slope_draws,
    probs = c((1 - conf) / 2, 1 - (1 - conf) / 2),
    names = FALSE
  )
  list(
    estimate = mean(slope_draws),
    standard_uncertainty = stats::sd(slope_draws),
    conf = conf,
    interval = stats::setNames(interval, c("lower", "upper")),
    draws = slope_draws
  )
}
