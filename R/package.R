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
"_PACKAGE"

# define global variables to pass package checks
## these variables are used in lazy evaluation or the shiny application
utils::globalVariables(
  c(
    "input",
    "session",
    "map_listener"
  )
)

# ensure package checks pass
#' @importFrom R.utils gzip
#' @importFrom rcbc cbc_solve
#' @importFrom Rsymphony Rsymphony_solve_LP
#' @importFrom future future
#' @importFrom geojsonsf geojson_sf
#' @importFrom leafem addHomeButton
#' @importFrom leafpop popupTable
#' @importFrom methods as
#' @importFrom RcppTOML parseToml
#' @importFrom tidyr gather
#' @importFrom withr with_options
#' @export
NULL
