% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matchr_matrix.R
\name{new_matrix}
\alias{new_matrix}
\title{Create a new matchr_matrix object}
\usage{
new_matrix(
  x = list(),
  x_ratios = list(),
  y_ratios = list(),
  x_files = list(),
  y_files = list(),
  x_total = integer(length = 1L),
  y_total = integer(length = 1L),
  x_na = character(),
  y_na = character()
)
}
\arguments{
\item{x}{A list of correlation matrices.}

\item{x_ratios}{A list of numeric vectors: the highest and lowest aspect
ratio in the x vector.}

\item{y_ratios}{A list of numeric vectors: the highest and lowest aspect
ratio in the y vector.}

\item{x_files}{A list of character vectors: the paths or URLs of the files
in the x vector.}

\item{y_files}{A list of character vectors: the paths or URLs of the files
in the y vector.}

\item{x_total}{An integer scalar: the total number of x signatures analyzed.}

\item{y_total}{An integer scalar: the total number of y signatures analyzed.}

\item{x_na}{A character vector: the paths of x signatures which are NA.}

\item{y_na}{A character vector: the paths of y signatures which are NA.}
}
\value{
An object of class \code{matchr_matrix}.
}
\description{
Create a new matchr_matrix object
}