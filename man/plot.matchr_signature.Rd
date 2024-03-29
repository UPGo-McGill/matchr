% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot.matchr_signature}
\alias{plot.matchr_signature}
\title{Plot a matchr_signature vector}
\usage{
\method{plot}{matchr_signature}(x, max_plot = 20, n_rows = "auto", ...)
}
\arguments{
\item{x}{Vector of class \code{matchr_signature}}

\item{max_plot}{Positive integer. The maximum number of signatures to plot at
once (default 20).}

\item{n_rows}{Either "auto" or a positive integer. The number of rows with
which to plot images. If "auto", nine or fewer images will be arranged in an
adaptive layout which maximizes legibility, while ten or more images will
always be arranged in a four-column grid.}

\item{...}{Not used.}
}
\description{
Plot a matchr_signature vector
}
