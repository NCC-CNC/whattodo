#' @include internal.R
NULL

#' Format error data
#'
#' Format results for a prioritization problem that threw an error.
#'
#' @inheritParams format_solution_results
#'
#' @inherit format_solution_results return
#'
#' @export
format_solution_error <- function(parameters) {
  # assert arguments are valid
  assertthat::assert_that(
    is.list(parameters)
  )

  # create default error data
  error_data <- tibble::tibble(
    name = parameters$error_sheet$sub_message
  )
  names(error_data) <- parameters$error_sheets$name_header

  # create summary table
  summary_data <- tibble::tibble(name = parameters$error_sheets$main_message)
  names(summary_data) <- parameters$error_sheets$name_header

  # return solution
  list(
    summary_results = summary_data,
    feature_results = error_data,
    site_results = error_data
  )
}
