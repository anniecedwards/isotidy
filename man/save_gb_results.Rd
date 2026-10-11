\name{save_gb_results}
\alias{save_gb_results}
\title{Save processed GB results and a human-readable processing report}
\usage{
save_gb_results(result, config, output_dir, prefix = NULL, verbose = TRUE)
}
\arguments{
\item{result}{Result returned by \code{process_gb()}.}

\item{config}{Experiment configuration used for processing.}

\item{output_dir}{Directory for CSV files and the HTML report.}

\item{prefix}{Optional filename prefix. Defaults to config$file_name.}

\item{verbose}{Print progress messages.}
}
\value{
A list of paths to the output files. Paths for unavailable optional outputs are NULL.
}
\description{
Writes the final processed GB data, optional replicate summary, exclusions,
and available drift/linearity/scale calibration diagnostics as CSV files.
It also renders the GB processing summary as an HTML report.
}
