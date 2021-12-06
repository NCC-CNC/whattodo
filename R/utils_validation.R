#' @include internal.R
NULL

#' Incorrect names
#'
#' Print the difference between two character vectors.
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
    is.character(y), assertthat::noNA(y)
  )
  # determine missing names from shapefile
  n1 <- setdiff(x, y)
  if (length(n1) == 0) {
    out1 <- ""
  } else {
    out1 <- paste0(
      "the shapefile is missing the following the names (that
      are present in the Excel Spreadsheet): ",
      paste0("\"", n1, "\""), "."
    )
  }
  # determine extra names in shapefile
  n2 <- setdiff(y, x)
  if (length(n2) == 0) {
    out2 <- ""
  } else {
    out2 <- paste0(
      "the shapefile has the following extra names (that
      are possibly misspelled from the Excel Spreadsheet): ",
      paste0("\"", n2, "\""), "."
    )
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
