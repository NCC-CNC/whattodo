#' @include internal.R
NULL

#' Is valid shapefile?
#'
#' Determine if a file is a valid shapefile?
#'
#' @param x `character` file path.
#'
#' @return `logical` indicating validity.
#'
#' @export
is_valid_shapefile <- function(x) {
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::noNA(x))
  r <- try(sf::read_sf(x), silent = TRUE)
  inherits(r, "sf")
}
