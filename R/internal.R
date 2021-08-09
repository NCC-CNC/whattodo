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

#' Column widths
#
#' Find column widths for a table.
#'
#' @param x `data.frame` object.
#'
#' @return `integer` value.
#'
#' @noRd
column_widths <- function(x) {
  assertthat::assert_that(inherits(x, "data.frame"))
  n <- nchar(names(x))
  n2 <- vapply(x, FUN.VALUE = numeric(1), function(y) {
    y[is.na(y)] <- 5
    max(nchar(as.character(y)))
  })
  pmax(n, n2)
}

#' Incorrect names
#'
#' Print incorrect names
#'
#' @param x `character` object.
#'
#' @param y `character` object.
#'
#' @return `character` description of missing names.
#'
#' @noRd
incorrect_names <- function(x, y) {
  # assert valid arguments
  assertthat::assert_that(
    is.character(x), assertthat::noNA(x),
    is.character(y),  assertthat::noNA(y))
  # determine missing names from shapefile
  n1 <- setdiff(x, y)
  if (length(n1) == 0) {
    out1 <- ""
  } else {
    out1 <- paste0(
      "the shapefile is missing the following the names (that
      are present in the Excel Spreadsheet): ",
      paste0("\"", n1, "\""), ".")
  }
  # determine extra names in shapefile
  n2 <- setdiff(y, x)
  if (length(n2) == 0) {
    out2 <- ""
  } else {
    out2 <- paste0(
      "the shapefile has the following extra names (that
      are possibly misspelled from the Excel Spreadsheet): ",
      paste0("\"", n2, "\""), ".")
  }
  # create output message
  if ((nchar(out1) == 0) && (nchar(out2) == 0)) {
    out <- ""
  } else if ((nchar(out1) > 0) && (nchar(out2) == 0)) {
    out <- paste0("Specifically, ", out1)
  } else if ((nchar(out1) == 0) && (nchar(out2) > 0)) {
    out <- paste0("Specifically, ", out2)
  } else if ((nchar(out1) > 0) && (nchar(out2) > 0)) {
    out <- paste0("Specifically, ", out1, " Also, ", out2)
  }
  # return result
  out
}

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
  if (length(x) == 0) return(TRUE)
  identical(anyDuplicated(x), 0L)
}
