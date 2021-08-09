#' @include internal.R
NULL

#' Has valid spreadsheet data?
#'
#' Check if a spreadsheet has valid data in it.
#'
#' @param x `list` object.
#'
#' @return `logical` indicating valid data.
#'
#' @export
has_valid_spreadsheet_data <- function(x) {
  # assert valid arguments
  assertthat::assert_that(inherits(x, "list"))
  # define helper function
  all_finite <- function(y) {
    all(vapply(as.list(y), function(z) all(!is.na(z)), logical(1)))
  }
  # return output
  all_finite(x$site_data) &&
  all_finite(x$feature_data) &&
  all_finite(x$site_status_data) &&
  all_finite(x$action_expectation_data)
}
