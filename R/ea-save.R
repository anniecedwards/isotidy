# Saving EA results: processed CSV plus rendered HTML processing report.

#' Save processed EA results and a human-readable HTML processing report
#'
#' Saves the final processed EA data and, when present, the replicate-averaged
#' visualization data as CSV files. It also renders the permanent EA
#' processing-summary R Markdown template as an HTML report.
#'
#' @param result Result returned by process_ea().
#' @param config Experiment configuration used for processing.
#' @param output_dir Directory where the CSV and HTML report will be saved.
#' @param prefix Optional filename prefix. Defaults to the experiment name.
#' @param verbose Print progress messages.
#'
#' @return A list containing paths to the data, optional visualization CSV,
#'   and HTML report.
#' @export
save_ea_results <- function(
    result,
    config,
    output_dir,
    prefix = NULL,
    verbose = TRUE
) {
  .validate_peripheral_save_inputs(result, config, output_dir)
  if (is.null(prefix)) prefix <- config$experiment_name
  if (is.null(prefix) || is.na(prefix) || !nzchar(prefix)) {
    stop("No filename prefix was supplied and config$experiment_name is missing.")
  }

  outputs <- list(
    data_file = list(data = result$output, suffix = "processed"),
    visualization_file = list(data = result$visualization_data, suffix = "visualization")
  )
  paths <- .save_peripheral_results(
    result = result,
    config = config,
    output_dir = output_dir,
    prefix = prefix,
    peripheral = "",
    outputs = outputs,
    report_params = "data_file",
    development_template = file.path("inst", "reports", "EA_processing_summary.Rmd"),
    package_template = "EA_processing_summary.Rmd",
    verbose = verbose,
    sanitize_prefix = FALSE,
    prefer_package_template = TRUE
  )

  if (verbose) message("Saved EA results to ", normalizePath(output_dir, winslash = "/", mustWork = FALSE))
  invisible(paths)
}
