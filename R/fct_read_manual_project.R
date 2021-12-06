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
#'   "extdata", "projects", "canada", "canada.xlsx",
#'   package = "whattodo"
#' )
#' f2 <- system.file(
#'   "extdata",  "projects", "canada", "canada.shp",
#'   package = "whattodo"
#' )
#' f3 <- system.file("extdata",  "config.toml", package = "whatdataio")
#'
#' # read project
#' x <- read_project(f1, yaml::yaml.load_file(f2), f3)
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
      x = spreadsheet_path, parameters = parameters
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
    ## attempt import
    spatial_data <- try(
      sf::read_sf(spatial_path),
      silent = TRUE
    )
    ## throw error if needed
    if (inherits(spatial_data, "try-error")) {
      error_msg <- c(
        "Failed to import spreadsheet data:",
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
      site_ids = speadsheet_data$site_ids,
      site_descriptions = speadsheet_data$site_descriptions,
      feature_ids = speadsheet_data$feature_ids,
      feature_descriptions = speadsheet_data$feature_descriptions,
      action_ids = speadsheet_data$action_ids,
      action_descriptions = speadsheet_data$action_descriptions,
      site_data = speadsheet_data$site_data,
      feasibility_data = speadsheet_data$feasibility_data,
      action_expectation_data = speadsheet_data$action_expectation_data,
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
    project = project,
  )
}
