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
  if (!is.list(result) || is.null(result$output)) {
    stop("'result' must be the list returned by process_gb().")
  }
  if (!is.list(config)) {
    stop("'config' must be the experiment configuration used by process_gb().")
  }
  if (length(output_dir) != 1 || is.na(output_dir) || !nzchar(output_dir)) {
    stop("'output_dir' must contain one non-empty path.")
  }
  if (is.null(prefix)) prefix <- config$file_name
  if (length(prefix) != 1 || is.na(prefix) || !nzchar(prefix)) {
    stop("Supply a non-empty 'prefix' or config$file_name.")
  }

  prefix <- gsub("[^A-Za-z0-9._-]+", "_", as.character(prefix))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  make_file <- function(suffix) {
    file.path(output_dir, paste0(prefix, "_gb_", suffix, ".csv"))
  }
  write_optional <- function(data, path) {
    if (is.null(data)) return(NULL)
    if (!is.data.frame(data)) {
      stop("Optional GB save outputs must be data frames.")
    }
    utils::write.csv(data, path, row.names = FALSE)
    normalizePath(path, winslash = "/", mustWork = FALSE)
  }

  data_file <- make_file("processed")
  visualization_file <- if (!is.null(result$visualization_data)) {
    make_file("visualization")
  } else {
    NULL
  }
  excluded_data_file <- if (!is.null(result$excluded_data)) make_file("excluded_data") else NULL
  excluded_analyses_file <- if (!is.null(result$excluded_analyses)) make_file("excluded_analyses") else NULL
  excluded_peaks_file <- if (!is.null(result$excluded_peaks)) make_file("excluded_peaks") else NULL
  drift_analyses <- result$drift_correction$standard_analyses
  drift_analyses_file <- if (is.data.frame(drift_analyses)) make_file("drift_standard_analyses") else NULL
  linearity_data <- result$linearity_model$model$model
  linearity_data_file <- if (is.data.frame(linearity_data)) make_file("linearity_calibration") else NULL
  scale_standards <- result$scale_model$stds_data
  scale_standards_file <- if (is.data.frame(scale_standards)) make_file("scale_standards") else NULL

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
  corrections_file <- make_file("corrections")

  utils::write.csv(result$output, data_file, row.names = FALSE)
  visualization_path <- write_optional(result$visualization_data, visualization_file)
  excluded_data_path <- write_optional(result$excluded_data, excluded_data_file)
  excluded_analyses_path <- write_optional(result$excluded_analyses, excluded_analyses_file)
  excluded_peaks_path <- write_optional(result$excluded_peaks, excluded_peaks_file)
  drift_analyses_path <- write_optional(drift_analyses, drift_analyses_file)
  linearity_data_path <- write_optional(linearity_data, linearity_data_file)
  scale_standards_path <- write_optional(scale_standards, scale_standards_file)
  utils::write.csv(corrections, corrections_file, row.names = FALSE)

  report_file <- file.path(
    output_dir,
    paste0(prefix, "_gb_processing_summary.html")
  )
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop(
      "The 'rmarkdown' package is required to create the HTML report. ",
      "Install it with install.packages('rmarkdown')."
    )
  }

  development_template <- file.path("inst", "reports", "GB_processing_summary.Rmd")
  template <- if (file.exists(development_template)) {
    normalizePath(development_template, winslash = "/", mustWork = TRUE)
  } else {
    system.file("reports", "GB_processing_summary.Rmd", package = "isotidy")
  }
  if (!nzchar(template) || !file.exists(template)) {
    stop("Could not find the GB processing report template. Expected: inst/reports/GB_processing_summary.Rmd")
  }

  if (verbose) message("Saving processed GB results to ", normalizePath(output_dir, winslash = "/", mustWork = FALSE))
  rmarkdown::render(
    input = template,
    output_file = basename(report_file),
    output_dir = output_dir,
    params = list(
      result = result,
      config = config,
      data_file = data_file,
      corrections_file = corrections_file
    ),
    envir = new.env(parent = globalenv()),
    quiet = !verbose
  )

  invisible(list(
    data_file = normalizePath(data_file, winslash = "/", mustWork = FALSE),
    visualization_file = visualization_path,
    excluded_data_file = excluded_data_path,
    excluded_analyses_file = excluded_analyses_path,
    excluded_peaks_file = excluded_peaks_path,
    drift_analyses_file = drift_analyses_path,
    linearity_calibration_file = linearity_data_path,
    scale_standards_file = scale_standards_path,
    corrections_file = normalizePath(corrections_file, winslash = "/", mustWork = FALSE),
    report_file = normalizePath(report_file, winslash = "/", mustWork = FALSE)
  ))
}
