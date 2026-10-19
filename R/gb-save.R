# Saving GB results: corrected peak data, calibration diagnostics, and report.

#' Save processed GB results and a human-readable processing report
#'
#' Writes the final processed GB data, optional replicate summary, exclusions,
#' and available drift/linearity/scale calibration diagnostics as CSV files.
#' It also renders the GB processing summary as an HTML report.
#'
#' @param result Result returned by process_gb().
#' @param config Experiment configuration used for processing.
#' @param output_dir Directory for CSV files and the HTML report.
#' @param prefix Optional filename prefix. Defaults to config$file_name.
#' @param verbose Print progress messages.
#'
#' @return A list of paths to the output files. Paths for unavailable optional
#'   outputs are NULL.
#' @export
save_gb_results <- function(
    result,
    config,
    output_dir,
    prefix = NULL,
    verbose = TRUE
) {
  .validate_peripheral_save_inputs(result, config, output_dir)
  if (is.null(prefix)) prefix <- config$file_name
  if (is.null(prefix) || is.na(prefix) || !nzchar(prefix)) {
    stop("Supply a non-empty 'prefix' or config$file_name.")
  }

  drift_analyses <- result$drift_correction$standard_analyses
  linearity_data <- result$linearity_model$model$model
  scale_standards <- result$scale_model$stds_data

  correction_rows <- list()
  add_correction <- function(stage, status, model = NULL, slope = NA_real_, intercept = NA_real_) {
    if (!is.null(model)) {
      slope <- model$slope %||% slope
      intercept <- model$intercept %||% intercept
    }
    correction_rows[[length(correction_rows) + 1L]] <<- data.frame(
      correction = stage,
      applied = isTRUE(status$applied),
      reason = as.character(status$reason %||% NA_character_),
      slope = as.numeric(slope),
      intercept = as.numeric(intercept),
      r_squared = as.numeric(model$r_squared %||% NA_real_),
      n_calibration_rows = as.integer(model$n_stds %||% model$n_standard_analyses %||% NA_integer_),
      stringsAsFactors = FALSE
    )
  }
  add_correction(
    "drift",
    result$drift_correction,
    slope = result$drift_correction$slope_per_analysis,
    intercept = result$drift_correction$intercept
  )
  add_correction("linearity", result$linearity_correction, result$linearity_model)
  add_correction("scale", result$scale_correction, result$scale_model)
  corrections <- do.call(rbind, correction_rows)

  outputs <- list(
    data_file = list(data = result$output, suffix = "processed"),
    visualization_file = list(data = result$visualization_data, suffix = "visualization"),
    excluded_data_file = list(data = result$excluded_data, suffix = "excluded_data"),
    excluded_analyses_file = list(data = result$excluded_analyses, suffix = "excluded_analyses"),
    excluded_peaks_file = list(data = result$excluded_peaks, suffix = "excluded_peaks"),
    drift_analyses_file = list(data = drift_analyses, suffix = "drift_standard_analyses"),
    linearity_calibration_file = list(data = linearity_data, suffix = "linearity_calibration"),
    scale_standards_file = list(data = scale_standards, suffix = "scale_standards"),
    corrections_file = list(data = corrections, suffix = "corrections")
  )

  if (verbose) message("Saving processed GB results to ", normalizePath(output_dir, winslash = "/", mustWork = FALSE))
  invisible(.save_peripheral_results(
    result = result,
    config = config,
    output_dir = output_dir,
    prefix = prefix,
    peripheral = "GB",
    outputs = outputs,
    report_params = c("data_file", "corrections_file"),
    development_template = file.path("inst", "reports", "GB_processing_summary.Rmd"),
    package_template = "GB_processing_summary.Rmd",
    verbose = verbose
  ))
}
