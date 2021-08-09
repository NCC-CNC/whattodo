#' @include internal.R
NULL

#' Parse site data
#'
#' Extract site data from \pkg{rhandsontable}.
#'
#' @param x `rhandsontable` object.
#'
#' @return x `data.frame` object.
#'
#' @export
parse_site_data <- function(x) {
  rhandsontable::hot_to_r(x)
}

#' Render status data
#'
#' Extract site status data from \pkg{rhandsontable}.
#'
#' @inheritParams parse_site_data
#'
#' @inherit parse_site_data return
#'
#' @export
parse_site_status_data <- function(x) {
  # extract data.frame table from rhandsontable
  x <- rhandsontable::hot_to_r(x)
  # convert logical columns to binary numeric values
  for (i in seq(2, ncol(x)))
    x[[i]] <- as.numeric(x[[i]])
  # return result
  x
}

#' Parse feature data
#'
#' Extract feature data from \pkg{rhandsontable}.
#'
#' @inheritParams parse_site_data
#'
#' @inherit parse_site_data return
#'
#' @export
parse_feature_data <- function(x) {
  rhandsontable::hot_to_r(x)
}

#' Parse action expectation data
#'
#' Extract action expectation data from \pkg{rhandsontable}.
#' @inheritParams parse_site_data
#'
#' @inherit parse_site_data return
#'
#' @export
parse_action_expectation_data <- function(x) {
  rhandsontable::hot_to_r(x)
}
