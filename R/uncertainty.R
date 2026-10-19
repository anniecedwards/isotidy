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
  if (sum(centered_x^2) <= 0) {
    stop("'x' must contain at least two distinct values.")
  }
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

#' Propagate GC calibration and replicate uncertainty to a plotted slope
#'
#' Uses the GC result returned by `process_gc()`. External-standard offsets
#' are drawn once per date and reference values once per compound. When NACME
#' correction was applied, its standard and certified-value uncertainties are
#' drawn and propagated through the carbon-balance equation. Sample injection
#' means are bootstrapped within each plotted point unless an explicit point
#' uncertainty is supplied.
#'
#' @param result Result returned by `process_gc()`.
#' @param points Data frame with one row per plotted point, grouping columns,
#'   an x variable, and optionally `measurement_uncertainty` (standard
#'   uncertainty of that point in final-value units).
#' @param x_column Name of the numeric predictor column in `points`.
#' @param group_columns Columns identifying a plotted point in both `points`
#'   and the processed GC data. Defaults to sample and compound.
#' @param nsim Number of Monte Carlo draws.
#' @param conf Confidence level for the slope interval.
#' @return List containing point estimates, simulated final values, slope
#'   uncertainty summary, and components that could not be included.
#' @export
propagate_gc_slope_uncertainty <- function(
    result,
    points,
    x_column,
    group_columns = c("sample_id", "compound_id"),
    nsim = 10000L,
    conf = 0.95
) {
  if (!is.list(result) || !is.data.frame(result$data)) {
    stop("'result' must be returned by process_gc() and contain data.")
  }
  if (!is.data.frame(points) || !nrow(points)) {
    stop("'points' must be a non-empty data frame.")
  }
  if (length(x_column) != 1L || !x_column %in% names(points) ||
      !is.numeric(points[[x_column]]) || any(!is.finite(points[[x_column]]))) {
    stop("'x_column' must name a finite numeric column in 'points'.")
  }
  if (length(nsim) != 1L || !is.finite(nsim) || nsim < 2 ||
      nsim != floor(nsim)) {
    stop("'nsim' must be an integer of at least 2.")
  }
  data <- result$data
  required_data <- c(
    "sample_id", "compound_id", "injection_id", "start_time", "delta_value",
    "delta_value_final"
  )
  missing_data <- setdiff(required_data, names(data))
  if (length(missing_data)) {
    stop("GC result data are missing: ", paste(missing_data, collapse = ", "))
  }
  missing_keys <- c(
    setdiff(group_columns, names(points)),
    setdiff(group_columns, names(data))
  )
  if (length(missing_keys)) {
    stop("Grouping columns are missing: ", paste(missing_keys, collapse = ", "))
  }
  if (!"compound_id" %in% group_columns) {
    stop("Include 'compound_id' in 'group_columns' so correction estimates ",
         "remain compound-specific.")
  }
  if (!all(group_columns %in% names(points)) ||
      anyNA(points[group_columns]) || anyDuplicated(points[group_columns])) {
    stop("'points' must have unique, non-missing grouping keys.")
  }

  sample_rows <- data
  if ("is_external_standard" %in% names(sample_rows)) {
    sample_rows <- sample_rows[
      is.na(sample_rows$is_external_standard) | !sample_rows$is_external_standard,
      , drop = FALSE
    ]
  }
  if ("is_derivatization_standard" %in% names(sample_rows)) {
    sample_rows <- sample_rows[
      is.na(sample_rows$is_derivatization_standard) |
        !sample_rows$is_derivatization_standard,
      , drop = FALSE
    ]
  }
  sample_rows$.correction_date <- as.Date(sample_rows$start_time)
  sample_rows <- sample_rows[
    is.finite(sample_rows$delta_value) &
      !is.na(sample_rows$.correction_date) &
      !is.na(sample_rows$injection_id) & !is.na(sample_rows$compound_id),
    , drop = FALSE
  ]
  if (!nrow(sample_rows)) stop("No eligible GC sample measurements were found.")

  injection_values <- sample_rows |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_columns)),
                    .data$.correction_date, .data$injection_id) |>
    dplyr::summarise(
      measured_mean = mean(.data$delta_value),
      observed_final_mean = mean(.data$delta_value_final, na.rm = TRUE),
      .groups = "drop"
    )
  points$.point_index <- seq_len(nrow(points))
  matched <- dplyr::inner_join(
    injection_values,
    points[, c(group_columns, ".point_index"), drop = FALSE],
    by = group_columns,
    relationship = "many-to-one"
  )
  if (nrow(matched) == 0L ||
      length(unique(matched$.point_index)) != nrow(points)) {
    stop("Every plotted point must match at least one processed sample injection.")
  }

  daily <- result$daily_offsets
  required_daily <- c("correction_date", "delta_offset", "standard_uncertainty")
  if (!is.data.frame(daily) ||
      length(setdiff(required_daily, names(daily)))) {
    stop("GC result lacks daily offset uncertainty estimates.")
  }
  daily$correction_date <- as.Date(daily$correction_date)
  if (any(!is.finite(daily$standard_uncertainty))) {
    stop("At least one daily external-standard offset lacks replicate precision.")
  }
  if (any(!matched$.correction_date %in% daily$correction_date)) {
    stop("Some plotted sample dates have no external-standard offset.")
  }

  missing_components <- character()
  ref_weights <- result$daily_reference_weights
  if (is.data.frame(ref_weights) && nrow(ref_weights)) {
    required_weights <- c(
      "correction_date", "compound_id", "reference_weight",
      "reference_delta", "reference_uncertainty"
    )
    missing_weights <- setdiff(required_weights, names(ref_weights))
    if (length(missing_weights)) {
      stop("GC result lacks external reference weighting fields: ",
           paste(missing_weights, collapse = ", "))
    }
    ref_weights$correction_date <- as.Date(ref_weights$correction_date)
    ref_weights$reference_uncertainty[!is.finite(ref_weights$reference_uncertainty)] <- 0
    if (any(!is.finite(result$daily_offsets$reference_uncertainty))) {
      missing_components <- c(missing_components,
        "external-standard certified-value uncertainty (missing in standards table)")
    }
  } else {
    missing_components <- c(missing_components,
      "external-standard certified-value uncertainty (reference weights unavailable)")
  }

  nacme_applied <- if (!is.null(result$nacme_correction_applied)) {
    isTRUE(result$nacme_correction_applied)
  } else {
    is.data.frame(result$derivatization_estimates) &&
      nrow(result$derivatization_estimates) > 0L
  }
  nacme <- NULL
  known_values <- NULL
  if (nacme_applied) {
    nacme <- result$derivatization_estimates
    required_nacme <- c(
      "correction_date", "compound_id", "delta_derivatized_mean",
      "sigma_derivatized", "delta_known", "sigma_known", "AA_C_count",
      "NACME_C_count"
    )
    missing_nacme <- setdiff(required_nacme, names(nacme))
    if (length(missing_nacme)) {
      stop("Applied NACME correction lacks uncertainty inputs: ",
           paste(missing_nacme, collapse = ", "))
    }
    if (any(!is.finite(nacme$sigma_derivatized)) ||
        any(!is.finite(nacme$delta_derivatized_mean)) ||
        any(nacme$sigma_derivatized < 0)) {
      stop("Applied NACME correction requires finite standard-injection precision.")
    }
    if (any(!is.finite(nacme$sigma_known))) {
      missing_components <- c(missing_components,
        "NACME certified-value uncertainty (missing in standards table)")
      nacme$sigma_known[!is.finite(nacme$sigma_known)] <- 0
    }
    if (any(nacme$sigma_known < 0)) {
      stop("NACME certified-value uncertainties cannot be negative.")
    }
    nacme$correction_date <- as.Date(nacme$correction_date)
    known_values <- unique(nacme[c("compound_id", "delta_known", "sigma_known")])
    if (anyDuplicated(known_values$compound_id)) {
      stop("NACME known values must be consistent within each compound.")
    }
    if (any(!matched$compound_id %in% nacme$compound_id)) {
      stop("Some plotted compounds lack an applied NACME estimate.")
    }
  }

  point_uncertainty <- if ("measurement_uncertainty" %in% names(points)) {
    as.numeric(points$measurement_uncertainty)
  } else {
    rep(NA_real_, nrow(points))
  }
  if (length(point_uncertainty) != nrow(points) ||
      any(!is.na(point_uncertainty) & !is.finite(point_uncertainty)) ||
      any(is.finite(point_uncertainty) & point_uncertainty < 0)) {
    stop("'measurement_uncertainty' must be non-negative or NA for each point.")
  }
  point_injections <- split(matched, matched$.point_index)
  if (any(vapply(point_injections, nrow, integer(1)) < 2L &
          !is.finite(point_uncertainty[as.integer(names(point_injections))]))) {
    stop("Each plotted point needs at least two sample injections or a finite ",
         "measurement_uncertainty column.")
  }

  reference_compounds <- character()
  if (is.data.frame(ref_weights) && nrow(ref_weights)) {
    reference_compounds <- unique(ref_weights$compound_id)
  }
  y_draws <- matrix(NA_real_, nrow = as.integer(nsim), ncol = nrow(points))
  for (simulation in seq_len(as.integer(nsim))) {
    offset_draw <- stats::rnorm(
      nrow(daily), daily$delta_offset, daily$standard_uncertainty
    )
    names(offset_draw) <- as.character(daily$correction_date)

    if (length(reference_compounds)) {
      reference_rows <- ref_weights[!duplicated(ref_weights$compound_id), ]
      reference_draw <- stats::rnorm(
        length(reference_compounds),
        reference_rows$reference_delta[match(reference_compounds,
                                              reference_rows$compound_id)],
        reference_rows$reference_uncertainty[match(reference_compounds,
                                                    reference_rows$compound_id)]
      )
      names(reference_draw) <- reference_compounds
      for (date in unique(ref_weights$correction_date)) {
        weights <- ref_weights[ref_weights$correction_date == date, ]
        offset_draw[as.character(date)] <- offset_draw[as.character(date)] +
          sum(weights$reference_weight *
                (reference_draw[weights$compound_id] - weights$reference_delta))
      }
    }

    nacme_draw <- NULL
    if (nacme_applied) {
      known_draw <- stats::rnorm(
        nrow(known_values), known_values$delta_known, known_values$sigma_known
      )
      names(known_draw) <- known_values$compound_id
      derivatized_draw <- stats::rnorm(
        nrow(nacme), nacme$delta_derivatized_mean, nacme$sigma_derivatized
      )
      total_carbon <- nacme$AA_C_count + nacme$NACME_C_count
      nacme_draw <- (total_carbon * derivatized_draw -
                       nacme$AA_C_count * known_draw[nacme$compound_id]) /
        nacme$NACME_C_count
      # The AA mix values used to estimate NACME received the same daily
      # external offset as samples. Preserve that covariance in the draws.
      nacme_dates <- as.character(nacme$correction_date)
      offset_index <- match(nacme$correction_date, daily$correction_date)
      if (anyNA(offset_index)) {
        stop("Applied NACME estimates contain dates without external offsets.")
      }
      observed_offsets <- daily$delta_offset[offset_index]
      nacme_draw <- nacme_draw +
        total_carbon / nacme$NACME_C_count *
        (offset_draw[nacme_dates] - observed_offsets)
    }

    for (point_index in seq_len(nrow(points))) {
      injections <- point_injections[[as.character(point_index)]]
      if (is.null(injections)) next
      if (is.finite(point_uncertainty[point_index])) {
        selected <- seq_len(nrow(injections))
      } else {
        selected <- sample.int(nrow(injections), replace = TRUE)
      }
      sampled <- injections[selected, , drop = FALSE]
      dates <- as.character(sampled$.correction_date)
      values <- sampled$measured_mean + offset_draw[dates]
      if (nacme_applied) {
        nacme_index <- match(
          paste(dates, sampled$compound_id),
          paste(nacme$correction_date, nacme$compound_id)
        )
        if (anyNA(nacme_index)) {
          stop("Applied NACME estimates do not cover all sample date/compound pairs.")
        }
        aa_count <- nacme$AA_C_count[nacme_index]
        nacme_count <- nacme$NACME_C_count[nacme_index]
        values <- ((aa_count + nacme_count) * values -
                     nacme_count * nacme_draw[nacme_index]) / aa_count
      }
      extra_measurement_error <- if (is.finite(point_uncertainty[point_index])) {
        stats::rnorm(1, 0, point_uncertainty[point_index])
      } else {
        0
      }
      y_draws[simulation, point_index] <- mean(values) +
        extra_measurement_error
    }
  }

  point_estimates <- matched |>
    dplyr::group_by(.data$.point_index) |>
    dplyr::summarise(
      estimate = mean(.data$observed_final_mean, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$.point_index)
  point_output <- points[order(points$.point_index), , drop = FALSE]
  point_output$estimate <- point_estimates$estimate
  point_output$standard_uncertainty <- apply(y_draws, 2, stats::sd)
  point_interval <- t(apply(y_draws, 2, stats::quantile,
                            probs = c((1 - conf) / 2, 1 - (1 - conf) / 2),
                            names = FALSE))
  point_output$lower <- point_interval[, 1]
  point_output$upper <- point_interval[, 2]
  point_output$.point_index <- NULL
  slope <- summarize_slope_uncertainty(
    x = points[[x_column]], y_draws = y_draws, conf = conf
  )
  list(
    point_estimates = point_output,
    y_draws = y_draws,
    slope = slope,
    nacme_applied = nacme_applied,
    components_not_included = unique(missing_components)
  )
}
