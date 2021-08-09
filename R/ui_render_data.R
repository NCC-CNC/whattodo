#' @include internal.R
NULL

#' Render status data
#'
#' Display site status data using \pkg{rhandsontable}.
#'
#' @param x `data.frame` object.
#'
#' @inheritParams format_pu_data
#'
#' @return A `rhandsontable` object.
#'
#' @export
render_site_status_data <- function(x, parameters) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "data.frame"), is.list(parameters))
  # convert binary values to logical
  for (i in seq(2, ncol(x))) {
    x[[i]] <- as.logical(x[[i]])
  }
  # initialize table
  r <- rhandsontable::rhandsontable(x, useTypes = TRUE)
  r <- rhandsontable::hot_col(r, col = 1, readOnly = TRUE)
  r <- rhandsontable::hot_col(
    r, col = seq(2, ncol(x)), type = "checkbox", renderer = paste0("
    function (instance, td, row, col, prop, value, cellProperties) {
       Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
       if (instance.params) {
         if (col > 0) {
           if (value > 0.5) {
             td.style.background = '", parameters$true_style$bgFill, "';
           } else {
             td.style.background = '", parameters$false_style$bgFill, "';
           }
         }
       }
    }"))

  # return table
  r
}

#' Render site data
#'
#' Display site data using \pkg{rhandsontable}.
#'
#' @param x `data.frame` object.
#'
#' @return `rhandsontable` object.
#'
#' @export
render_site_data <- function(x) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "data.frame"))
  # initialize table
  r <- rhandsontable::rhandsontable(x, useTypes = TRUE)
  r <- rhandsontable::hot_col(r, col = c(1, 2, 3), readOnly = TRUE)
  r <- rhandsontable::hot_validate_numeric(
    r, seq(4, ncol(x)), min = 0, max = 1e+6, allowInvalid = FALSE)
  # return table
  r
}

#' Render feature data
#'
#' Display feature data using \pkg{rhandsontable}.
#'
#' @param x `data.frame` object.
#'
#' @export
render_feature_data <- function(x) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "data.frame"))
  # initialize table
  r <- rhandsontable::rhandsontable(x, useTypes = TRUE)
  r <- rhandsontable::hot_col(r, col = 1, readOnly = TRUE)
  r <- rhandsontable::hot_validate_numeric(
    r, 2, min = 0, max = 1e+6, allowInvalid = FALSE)
  r <- rhandsontable::hot_validate_numeric(
    r, 3, min = 0, max = 100, allowInvalid = FALSE)
  # return table
  r
}

#' Render action expectation data
#'
#' Display action expectation data using \pkg{rhandsontable}.
#'
#' @param x `data.frame` object.
#'
#' @export
render_action_expectation_data <- function(x) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "data.frame"))
  # initialize table
  r <- rhandsontable::rhandsontable(x, useTypes = TRUE)
  r <- rhandsontable::hot_col(r, col = 1, readOnly = TRUE)
  r <- rhandsontable::hot_validate_numeric(
    r, seq(2, ncol(x)), min = 0, max = 1e+6, allowInvalid = FALSE)
  # return table
  r
}

#' Render summary results data
#'
#' Display summary results data using \pkg{rhandontable}.
#'
#' @param x `data.frame` object.
#'
#' @param parameters `list` object.
#'
#' @export
render_summary_results_data <- function(x, parameters) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, "data.frame"), is.list(parameters))
  # main processing
  if (!identical(x[[1]][[1]], parameters$error_sheets$main_message)) {
    # use defaults if not showing specific error message
    out <-
      rhandsontable::hot_cols(
        rhandsontable::rhandsontable(x, useTypes = TRUE), readOnly = TRUE)
  } else {
    # manually specify column width
    out <-
      rhandsontable::hot_cols(
        rhandsontable::rhandsontable(x, useTypes = TRUE),
          readOnly = TRUE, colWidths = rep(300, ncol(x)))
  }
  out
}

#' Render feature results data
#'
#' Display summary feature data using \pkg{rhandontable}.
#'
#' @param x `data.frame` object.
#'
#' @param parameters `list` object.
#'
#' @export
render_feature_results_data <- function(x, parameters) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, "data.frame"), is.list(parameters))
  # main processing
  out <-
    rhandsontable::hot_cols(
      rhandsontable::rhandsontable(x, useTypes = TRUE),
      readOnly = TRUE, colWidths = c(300, rep(200, ncol(x) - 1)))
  out
}
