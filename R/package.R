#' @include internal.R
NULL

#' @importFrom magrittr %>%
#' @import shinyBS
#' @import sf
NULL

#' whattodo: Interactive management action prioritization application
#'
#' The What To Do application is a decision support tool to help prioritize
#' management actions for the Nature Conservancy of Canada. Data can be
#' uploaded using an Excel Spreadsheet and (optional) a shapefile delineating
#' the spatial location of sites. Prioritizations are generated using mixed
#' integer programming techniques. The performance of candidate prioritizations
#' can be examined using summary statistics and tables. Finally, data and
#' prioritizations can also be downloaded for subsequent analysis.
#'
#' @name whattodo
#'
#' @docType package
#'
#' @examples
#' \donttest{
#  # launch application
#' if (interactive()) {
#' run_app()
#' }
#' }
NULL

# define functions for internally used packages to pass checks
tmp1 <- rcbc::cbc_solve
