#' @include internal.R
NULL

#' @import promises
#' @import shinyBS
#' @import sf
#' @import R6
#' @importFrom magrittr %>%

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

# define global variables to pass package checks
## these variables are used in lazy evaluation or the shiny application
utils::globalVariables(
  c(
    "input",
    "session",
    "map_listener"
  )
)

# define functions for internally used packages to pass checks
tmp1 <- R.utils::gzip
tmp1 <- rcbc::cbc_solve
tmp1 <- Rsymphony::Rsymphony_solve_LP
