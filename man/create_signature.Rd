% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_signature.R
\name{create_signature}
\alias{create_signature}
\alias{create_signature.matchr_image}
\alias{create_signature.character}
\title{Generate a unique colour signature for an image}
\usage{
create_signature(image, bands = 20, rm_black_bars = TRUE, ...)

\method{create_signature}{matchr_image}(image, bands = 20, rm_black_bars = TRUE, quiet = FALSE, ...)

\method{create_signature}{character}(
  image,
  bands = 20,
  rm_black_bars = TRUE,
  backup = TRUE,
  quiet = FALSE,
  ...
)
}
\arguments{
\item{image}{Vector of class \code{matchr_image} (imported using
\code{\link{load_image}}), or character vector of file paths or URLs which
can be imported to \code{matchr_image} using \code{\link{load_image}}.}

\item{bands}{Integer scalar. The number of horizontal and vertical bands the
image should be split into for processing. Higher values will produce a more
distinctive colour signature, potentially decreasing the rate of matching
false positives, but at the cost of increased processing time and an
increased rate of matching false negatives.}

\item{rm_black_bars}{Logical scalar. Should horizontal black bars be
detected and removed from the image signature? Because these bands lead to an
image signature dominated by black values, leaving them in the signature can
lead to false positive matches.}

\item{...}{Additional arguments passed to methods.}

\item{quiet}{A logical scalar. Should the function execute quietly, or should
it return status updates throughout the function (default)?}

\item{backup}{A logical scalar. Should the function store an ongoing backup
of progress in a hidden object \code{.matchr_env$sig_backup} in the package's
environment (default)? If TRUE, the function will attempt to resume progress
if it detects a previous backup, and it will remove the backup if the
function successfully completes. Backups can be removed with
\code{\link{remove_backups}}.}
}
\value{
A vector of class \code{matchr_signature} of the same length as the input
vector.
}
\description{
\code{create_signature} takes an image file and generates a numeric vector of
average greyscale and colour values, so that the image can be compared to
others.
}
\details{
An image is decomposed into horizontal and vertical bands (with the number of
bands controlled by the \code{bands} argument), and for each band an average
greyscale and colour value is calculated. The vector of these averages
becomes a distinctive signature that can identify a given image even if the
image is rescaled or compressed, and thus serves as a reliable indicator of
whether two images are the same.
}
\examples{
# Import image with load_image then create signature
img <- load_image("https://upgo.lab.mcgill.ca/img/UPGo_logo.png")
create_signature(img)

# Or create signature directly from path/URL
create_signature("https://upgo.lab.mcgill.ca/img/UPGo_logo.png")

# By default top and bottom black bars are removed, but leave them with
# rm_black_bars = FALSE
create_signature(img, rm_black_bars = FALSE)
}
