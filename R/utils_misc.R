#' @include internal.R
NULL

#' Is false?
#'
#' Determine if an object is identical to `FALSE`.
#'
#' @param x an object.
#'
#' @return `logical` indicating if object is identical to `FALSE`.
#'
#' @export
isFALSE <- function(x) {
  identical(x, FALSE)
}

#' Has valid values?
#'
#' Check if a vector has valid values?
#'
#' @param x `character` `vector` object.
#'
#' @return `logical` value.
#'
#' @export
has_valid_values <- function(x) {
  isTRUE((length(x) > 0) && assertthat::noNA(x) && all(nchar(x) > 0))
}

#' Has duplicates?
#'
#' Check if a vector has duplicates?
#'
#' @param x `character` `vector` object.
#'
#' @return `logical` value.
#'
#' @export
has_unique_values <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(TRUE)
  }
  identical(anyDuplicated(x), 0L)
}
