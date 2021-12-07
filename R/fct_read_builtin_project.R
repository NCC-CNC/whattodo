#' @include internal.R
NULL

#' Read builtin project
#'
#' Read a project from disk.
#'
#' @param path `character` file path for the project YAML metadata file.
#'
#' @inheritParams read_manual_project
#'
#' @inherit read_manual_project return
#'
#' @examples
#' # specify file paths
#' f1 <- system.file(
#'   "extdata", "projects", "canada", "canada.yaml",
#'   package = "whattodo"
#' )
#' f2 <- system.file("extdata",  "config.toml", package = "whatdataio")
#'
#' # read project
#' x <- read_project(f1, yaml::yaml.load_file(f2))
#'
#' # print project
#' print(x)
#' @export
read_builtin_project <- function(path, parameters) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(path),
    assertthat::noNA(path),
    is.list(parameters)
  )

  # import project details
  x <- yaml::read_yaml(path)

  # set file paths if needed
  spreadsheet_path <- file.path(dirname(path), basename(x$spreadsheet_path))
  spatial_path <- file.path(dirname(path), basename(x$spatial_path))

  # read project
  project <- read_manual_project(
    spreadsheet_path = spreadsheet_path,
    spatial_path = spatial_path,
    name = x$name,
    author_name = x$author_name,
    author_email = x$author_email,
    parameters = parameters
  )
}
