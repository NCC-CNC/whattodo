#' @include internal.R
NULL

#' Read project
#'
#' Read a project from disk.
#'
#' @param spreadsheet_path `character` file path for the spreadsheet data.
#'
#' @param parameters `list` object to interpret spreadsheet data.
#'
#' @param spatial_path `character` file path for the spatial data.
#'  Defaults to `NULL` such that no spatial data are imported.
#'
#' @param name `character` name for the project.
#'  Defaults to a value derived from the argument to `spreadsheet_path`.
#'
#' @param author_name `character` name for the author of the  project.
#'  Defaults to a parameter specified in the Golem configuration file.
#'
#' @param author_email `character` contact email for the project.
#'  Defaults to a parameter specified in the Golem configuration file.
#'
#' @return A `list` containing the following elements:
#' \describe{
#' \item{name}{A `character` value indicating the name.}
#' \item{author_name}{A `character` indicating the author name for the data.}
#' \item{author_email}{A `character` indicating the contact email for the data.}
#' \item{project}{A [Project] object.}
#' }
#'
#' @examples
#' # specify file paths
#' f1 <- system.file(
#'   "extdata", "projects", "simulated-data", "simulated-data.xlsx",
#'   package = "whattodo"
#' )
#' f2 <- system.file(
#'   "extdata",  "projects", "simulated-data", "simulated-data.shp",
#'   package = "whattodo"
#' )
#'
#' # read project
#' x <- read_manual_project(f1, whatdataio::read_data_configuration(), f2)
#'
#' # print project
#' print(x)
#' @export
read_manual_project <- function(spreadsheet_path,
                                parameters,
                                spatial_path = NULL,
                                name = tools::file_path_sans_ext(
                                  basename(spreadsheet_path)
                                ),
                                author_name =
                                  get_golem_config("default_project_name"),
                                author_email =
                                  get_golem_config("default_project_email")) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(name),
    assertthat::noNA(name),
    assertthat::is.string(author_name),
    assertthat::noNA(author_name),
    assertthat::is.string(author_email),
    assertthat::noNA(author_email),
    assertthat::is.string(spreadsheet_path),
    assertthat::noNA(spreadsheet_path),
    is.list(parameters)
  )

  # import spreadsheet data
  ## attempt import
  spreadsheet_data <- try(
    whatdataio::read_spreadsheet_data(
      x = spreadsheet_path,
      parameters = parameters
    ),
    silent = TRUE
  )
  ## throw error if needed
  if (inherits(spreadsheet_data, "try-error")) {
    error_msg <- c(
      "Failed to import spreadsheet data:",
      as.character(spreadsheet_data),
      ""
    )
    error_msg <- simpleError(paste(error_msg, collapse = "\n"))
    return(error_msg)
  }

  # import spatial data
  if (!is.null(spatial_path)) {
    ## unzip data if needed
    if (endsWith(spatial_path, ".zip")) {
      tmp_dir <- tempfile()
      dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
      utils::unzip(spatial_path, exdir = tmp_dir)
      spatial_path <- dir(
        tmp_dir, "^.*\\.shp", full.names = TRUE, recursive = TRUE
      )
    }
    ## attempt import
    spatial_data <- try(
      sf::read_sf(spatial_path),
      silent = TRUE
    )
    ## throw error if needed
    if (inherits(spatial_data, "try-error")) {
      error_msg <- c(
        "Failed to import spatial data:",
        as.character(spatial_data),
        ""
      )
      error_msg <- simpleError(paste(error_msg, collapse = "\n"))
      return(error_msg)
    }
  } else {
    spatial_data <- NULL
  }

  # create project
  ## attempt creation
  project <- try(
    new_project(
      site_ids = spreadsheet_data$site_ids,
      site_descriptions = spreadsheet_data$site_descriptions,
      feature_ids = spreadsheet_data$feature_ids,
      feature_descriptions = spreadsheet_data$feature_descriptions,
      action_ids = spreadsheet_data$action_ids,
      action_descriptions = spreadsheet_data$action_descriptions,
      site_data = spreadsheet_data$site_data,
      feature_data = spreadsheet_data$feature_data,
      feasibility_data = spreadsheet_data$feasibility_data,
      consequence_data = spreadsheet_data$consequence_data,
      site_geometry = spatial_data,
      parameters = parameters
    ),
    silent = TRUE
  )
  ## throw error if needed
  if (inherits(project, "try-error")) {
    error_msg <- c(
      "Failed to create project using spreadsheet and spatial data:",
      as.character(project),
      ""
    )
    error_msg <- simpleError(paste(error_msg, collapse = "\n"))
    return(error_msg)
  }

  # return result
  list(
    name = name,
    author_name = author_name,
    author_email = author_email,
    project = project
  )
}
