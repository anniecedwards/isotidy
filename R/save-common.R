# Shared save and report-rendering helpers used by peripheral save functions.

.validate_peripheral_save_inputs <- function(result, config, output_dir) {
  if (!is.list(result) || is.null(result$output)) {
    stop("'result' must be a completed result list from a peripheral pipeline.")
  }
  if (!is.list(config)) {
    stop("'config' must be the experiment configuration used for processing.")
  }
  if (length(output_dir) != 1 || is.na(output_dir) || !nzchar(output_dir)) {
    stop("'output_dir' must contain one non-empty path.")
  }
}

.save_peripheral_results <- function(
    result,
    config,
    output_dir,
    prefix,
    peripheral,
    outputs,
    report_params,
    development_template,
    package_template,
    verbose = TRUE,
    sanitize_prefix = TRUE,
    prefer_package_template = FALSE
) {
  .validate_peripheral_save_inputs(result, config, output_dir)
  if (length(prefix) != 1 || is.na(prefix) || !nzchar(prefix)) {
    stop("Supply a non-empty filename prefix.")
  }
  if (sanitize_prefix) {
    prefix <- gsub("[^A-Za-z0-9._-]+", "_", as.character(prefix))
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  file_stem <- if (nzchar(peripheral)) {
    paste0(prefix, "_", tolower(peripheral), "_")
  } else {
    paste0(prefix, "_")
  }

  output_paths <- lapply(names(outputs), function(key) {
    data <- outputs[[key]]$data
    suffix <- outputs[[key]]$suffix
    if (is.null(data)) return(NULL)
    if (!is.data.frame(data)) {
      stop("Save output '", key, "' must be a data frame or NULL.")
    }
    path <- file.path(output_dir, paste0(file_stem, suffix, ".csv"))
    utils::write.csv(data, path, row.names = FALSE)
    normalizePath(path, winslash = "/", mustWork = FALSE)
  })
  names(output_paths) <- names(outputs)

  installed_template <- system.file(
    "reports", package_template, package = "isotidy"
  )
  template_candidates <- if (prefer_package_template) {
    c(installed_template, development_template)
  } else {
    c(development_template, installed_template)
  }
  existing_templates <- template_candidates[
    nzchar(template_candidates) & file.exists(template_candidates)
  ]
  template <- if (length(existing_templates)) {
    normalizePath(existing_templates[[1]], winslash = "/", mustWork = TRUE)
  } else {
    ""
  }
  if (!nzchar(template) || !file.exists(template)) {
    stop("Could not find the ", peripheral, " processing report template. ",
         "Expected: ", development_template)
  }
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop(
      "The 'rmarkdown' package is required to create the HTML report. ",
      "Install it with install.packages('rmarkdown')."
    )
  }

  report_file <- file.path(
    output_dir,
    paste0(file_stem, "processing_summary.html")
  )
  params <- c(list(result = result, config = config), output_paths[report_params])
  rmarkdown::render(
    input = template,
    output_file = basename(report_file),
    output_dir = output_dir,
    params = params,
    envir = new.env(parent = globalenv()),
    quiet = !verbose
  )

  c(
    output_paths,
    list(report_file = normalizePath(report_file, winslash = "/", mustWork = FALSE))
  )
}
