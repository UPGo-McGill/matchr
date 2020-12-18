
<!-- README.md is generated from README.Rmd. Please edit that file -->

# matchr

<!-- badges: start -->

[![R build
status](https://github.com/UPGo-McGill/matchr/workflows/R-CMD-check/badge.svg)](https://github.com/UPGo-McGill/matchr/actions)
[![codecov](https://codecov.io/gh/UPGo-McGill/matchr/branch/master/graph/badge.svg)](https://codecov.io/gh/UPGo-McGill/matchr)
<!-- badges: end -->

The goal of matchr is to facilitate fast and reliable comparison of
large sets of images to identify identical or nearly-identical pairs. It
works by generating distinctive image signatures from pixel data then
correlating these signatures between sets of images.

Image are decomposed into horizontal bands, and for each band an average
greyscale or colour value is calculated. The vector of these averages
becomes a distinctive signature that can identify a given image even if
the image is rescaled or compressed, and thus serves as a reliable
indicator of whether two images are the same.

Using matrix algebra, matchr can compare multiple sets of images and
identify likely matches. The method is robust to changes in tint,
compression or aspect ratio, and to other minor differences.

The package includes a Shiny app (`compare_matches`) for visually
inspecting match results and confirm or reject the matches.

All time-consuming matchr functions can be parallelized through the
[future](https://cran.r-project.org/web/packages/future/vignettes/future-1-overview.html)
package, can report progress through the
[progressr](https://cran.r-project.org/web/packages/progressr/vignettes/progressr-intro.html)
package, and can save incremental backups in case of an error.

## Installation

(NOT YET WORKING) You can install the released version of matchr from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("matchr")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("UPGo-McGill/matchr")
```

## Usage

The standard matchr flow involves importing one or more sets of images
with `load_image`, generating image signatures from the image sets using
`create_signature`, matching image signatures using `match_signatures`,
then refining and verifying the matches using `confirm_matches`,
`compare_images`, and `integrate_changes`.

Because each of these steps can be very time- or computation-intensive,
it is usually the most convenient to run these functions separately. But
in the case of relatively small comparison tasks, `match_images`
provides an “all-in-one” function which performs each task sequentially
and delivers the final results.

When the {shiny} package is installed, `compare_images` loads an
interactive Shiny app for verifying the results of the comparison
algorithm; otherwise, images are loaded statically in a viewer window
for manual comparison.

## Learn more

Use `vignette(package = "matchr")` to learn more about how matchr
works\!
