#' @include internal.R
NULL

#' Geometry data type
#'
#' Determine type of geometry in `sf` object.
#'
#' @param x `sf` object.
#'
#' @return `character` description of data type.
#'
#' @export
st_geometry_data_type <- function(x) {
  assertthat::assert_that(inherits(x, "sf"))
  x <- sf::st_geometry(x)
  # from sf:::sfc.print
  substr(class(x)[1], 5, nchar(class(x)[1]))
}
