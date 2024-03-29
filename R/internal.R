#' All elements inherit
#'
#' Check if all elements in a `list` object inherit from a class.
#'
#' @param x `list` object.
#'
#' @param class `character` class name.
#
#' @return `logical` indicating if all elements in `x` inherit from `class`.
#'
#' @noRd
all_list_elements_inherit <- function(x, class) {
  assertthat::assert_that(
    is.list(x),
    is.character(class),
    assertthat::noNA(class)
  )
  all(vapply(x, inherits, logical(1), class))
}

assertthat::on_failure(all_list_elements_inherit) <- function(call, env) {
  paste0("all ", deparse(call$x), " do not inherit from", deparse(call$class))
}

#' Paste vector
#'
#' Paste a `character` vector of together for displaying messages.
#'
#' @param x `character` vector.
#'
#' @return `character` value.
paste_vector <- function(x) {
  assertthat::assert_that(inherits(x, "character"))
  paste(paste0("\"", x, "\""), collapse = ", ")
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

#' Convert to identifier
#'
#' Convert a name to a HTML identifier.
#'
#' @param x `character` vector containing identifiers.
#'
#' @return A `character` value.
convert_to_html_id <- function(x) {
  assertthat::assert_that(
    is.character(x),
    assertthat::noNA(x)
  )
  # replace spaces with underscores
  gsub(" ", "_", x, fixed = TRUE)
}

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
