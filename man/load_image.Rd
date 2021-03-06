% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_image.R
\name{load_image}
\alias{load_image}
\title{Load image from file or URL}
\usage{
load_image(file, quiet = FALSE)
}
\arguments{
\item{file}{A vector of file paths or URLs. If \code{file} is a vector of URLs,
the URLs must begin with "http", "https", "ftp" or "ftps". If the URL has no
extension, it will be given the extension ".jpg".}

\item{quiet}{A logical scalar. Should the function execute quietly, or should
it return status updates throughout the function (default)?}
}
\value{
A \code{matchr_image} vector of the same length as the input vector.
}
\description{
\code{load_image} is a vectorized method for reading bitmaps (jpegs, pngs and
bmps) into memory as arrays of pixel values. It optionally supports parallel
processing (via \code{future} and \code{future.apply}) and progress
reporting.
}
\details{
Because the memory requirements of storing image representations in memory
so large, it is usually not feasible to read in more than several hundred
images at a time with \code{load_image}. For these cases, use
\code{\link{create_signature}} directly on the input file paths. By default
this will read images with \code{load_image} 100 at a time before
generating the unique colour signatures used for image matching.
}
\examples{
load_image("https://upgo.lab.mcgill.ca/img/UPGo_logo.png")
}
