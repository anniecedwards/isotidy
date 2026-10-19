\name{process_gb}
\alias{process_gb}
\title{Process GB-IRMS data with drift, linearity, and scale calibration}
\usage{
process_gb(
  raw_df,
  config,
  schema = NULL,
  isotope = "C13",
  run_id = NULL,
  exclude_patterns = NULL,
  exclude_analysis_numbers = NULL,
  drift_standard_id = NULL,
  apply_drift_correction = TRUE,
  apply_linearity = TRUE,
  stds_reference_df = NULL,
  sample_peak_numbers = NULL,
  verbose = TRUE
)
}
\arguments{
\item{raw_df}{Raw GB-IRMS data frame.}
\item{config}{Experiment configuration returned by \code{load_IRMS_config()}.}
\item{schema}{Optional GB peripheral schema.}
\item{isotope}{Isotope system to process, such as \code{"C13"}.}
\item{run_id}{Optional run identifier. Defaults to the experiment name.}
\item{exclude_patterns}{Name patterns to exclude. Defaults to the GB patterns or config value.}
\item{exclude_analysis_numbers}{Analysis numbers to exclude. Defaults to \code{config$processing$exclude_analysis_numbers} or none.}
\item{drift_standard_id}{Standard used to estimate run drift. Defaults to \code{config$processing$drift_standard_id}; no lab-specific default is used.}
\item{apply_drift_correction}{Apply a linear analysis-number correction when at least three drift-standard analyses are available.}
\item{apply_linearity}{Apply area-dependent linearity correction using the configured calibration standard and selected sample peak range.}
\item{stds_reference_df}{Laboratory standards reference table. Defaults to \code{config$stds_reference_df}. Supplying it enables linearity and scale calibration; without it, those stages are reported as skipped.}
\item{sample_peak_numbers}{Optional explicit peak-number vector overriding the configured low/high range.}
\item{verbose}{Print progress information.}
}
\value{
A list containing selected sample peak rows and output, all retained peak rows,
rows excluded by name or analysis number, peaks outside the configured range,
and drift, linearity, and scale model diagnostics. Raw, drift-adjusted,
linearity-corrected, and final scale-calibrated values are retained when their
correction stages are available.
}
\description{
Adapts raw GB peak rows, applies configurable exclusions, selects the configured
sample peak range, then applies run drift correction, optional linearity
correction, and scale calibration in that order. Linearity fitting uses only
the selected peak range. Scale calibration uses one mean per standard
injection, so repeated peaks from one injection do not count as independent
standard measurements. Other peak rows remain available for review.
}
