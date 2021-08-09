#' @include internal.R
NULL

#' Default tabular data
#'
#' Create a place-holder table to display before any results have been
#' generated.
#'
#' @return A `data.frame` object.
#'
#' @export
default_tabular_data <- function() {
  tibble::tibble(
    Status = "No information to show.",
    Message = "Please click the \"Generate prioritization!\" button.")
}

#' Default spatial bounding box
#'
#' Create a default bounding box.
#'
#' @return `list` object.
#'
#' @export
default_bbox <- function() {
  sf::st_bbox(sf::st_sfc(sf::st_point(0:1), sf::st_point(0:1)), crs = 3857)
}

#' Default colors
#'
#' Generate a set of colors.
#'
#' @param x `character` vector of elements that require colors.
#'
#' @inheritParams default_color_matrix
#'
#' @return `character` vector of colors (one for each element in `x`)
#'
#' @export
default_colors <- function(x, parameters) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(x),
    identical(anyDuplicated(x), 0L),
    is.list(parameters))
  # get colors
  all_names <- parameters$map$actionColorNames
  all_codes <- parameters$map$actionColorCodes
  n <- length(x)
  # determine colors
  if (length(x) >= length(all_names)) {
    ## if all colors need to be used then do so
    out <- matrix("", nrow = 2, ncol = n)
    out[1, ] <- c(all_names, rep("black", n - length(all_names)))
    out[2, ] <- c(all_codes, rep("#000000", n - length(all_names)))
  } else {
    ## if only a subset of colors are needed, then find optimal color scheme
    rgb <- grDevices::col2rgb(all_codes)
    hsv <- grDevices::rgb2hsv(rgb)
    idx <- cluster::pam(t(hsv), length(x))$id.med
    out <- matrix("", nrow = 2, ncol = n)
    out[1, ] <- all_names[idx]
    out[2, ] <- all_codes[idx]
  }
  # return colors
  colnames(out) <- x
  out
}

#' Default color matrix
#'
#' Create a default matrix for specifying colors.
#'
#' @param n `intger` number of sites.
#'
#' @param parameters `list` object.
#'
#' @return `character` `matrix` object.
default_color_matrix <- function(n, parameters) {
  assertthat::assert_that(is.list(parameters))
  out <- matrix(
    c(parameters$map$defaultColorName, parameters$map$defaultColorCode),
    nrow = 2, ncol = n)
  colnames(out) <- rep("Site", ncol(out))
  out
}
