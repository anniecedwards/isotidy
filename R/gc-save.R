# Saving GC results: corrected peak data, correction diagnostics, and HTML report.

#' Save processed GC results and a processing summary
#'
#' Writes final corrected GC peak data, optional replicate-averaged
#' visualization data, exclusions, external-standard offsets, optional NACME
#' estimates, and an HTML processing summary.
#'
#' @param result Result returned by process_gc().
#' @param config Experiment configuration used for processing.
#' @param output_dir Directory for CSV files and the HTML report.
#' @param prefix Optional filename prefix. Defaults to config$file_name.
#' @param verbose Print progress messages.
#'
#' @return A list of paths to the output files, including the optional
#'   visualization CSV.
#' @export
save_gc_results <- function(
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

  derivatization_estimates <- result$derivatization_estimates
  derivatization_estimates <- if (
    is.data.frame(derivatization_estimates) && nrow(derivatization_estimates) > 0
  ) derivatization_estimates else NULL

  outputs <- list(
    data_file = list(data = result$output, suffix = "processed"),
    visualization_file = list(data = result$visualization_data, suffix = "visualization"),
    injection_offsets_file = list(data = result$injection_offsets, suffix = "injection_offsets"),
    daily_offsets_file = list(data = result$daily_offsets, suffix = "daily_offsets"),
    derivatization_estimates_file = list(data = derivatization_estimates, suffix = "nacme_estimates"),
    excluded_measurements_file = list(data = result$excluded_measurements, suffix = "excluded_measurements")
  )

  invisible(.save_peripheral_results(
    result = result,
    config = config,
    output_dir = output_dir,
    prefix = prefix,
    peripheral = "GC",
    outputs = outputs,
    report_params = c(
      "data_file", "injection_offsets_file", "daily_offsets_file",
      "excluded_measurements_file"
    ),
    development_template = file.path("inst", "reports", "GC_processing_summary.Rmd"),
    package_template = "GC_processing_summary.Rmd",
    verbose = verbose
  ))
}
