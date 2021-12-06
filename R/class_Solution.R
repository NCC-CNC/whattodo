#' @include internal.R
NULL

#' Solution class
#'
#' Definition for the Solution class.
#'
#' @seealso [new_project()].
Solution <- R6::R6Class(
  "Solution",
  public = list(

    #' @field id `character` identifier.
    id = NA_character_,

    #' @field project [Project] object.
    project = NULL,

    #' @field summary_results `data.frame` object.
    summary_results = NULL,

    #' @field site_results `data.frame` object.
    site_results = NULL,

    #' @field feature_results `data.frame` object.
    feature_results = NULL,

    #' @description
    #' Create a Solution object.
    #' @param id `character` identifier.
    #' @param project `character` identifier.
    #' @param summary_results `data.frame` object.
    #' @param site_results `data.frame` object.
    #' @param feature_results `data.frame` object.
    #' @return A new Solution object.
    initialize = function(id,
                          project,
                          summary_results,
                          site_results,
                          feature_results) {
      # assert that arguments are valid
      assertthat::assert_that(
        ## ids
        assertthat::is.string(id),
        assertthat::noNA(id),
        ## project
        inherits(project, "Project"),
        ## results
        inherits(summary_results, "data.frame"),
        inherits(site_results, "data.frame"),
        inherits(feature_results, "data.frame"),
      )

      # assign fields
      self$id <- id
      self$project <- project
      self$summary_results <- summary_results
      self$site_results <- site_results
      self$feature_results <- feature_results

    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message("Solution")
      message("  id:       ", self$id)
      message("  sites:    ", paste_vector(self$project$site_ids))
      message("  features: ", paste_vector(self$project$feature_ids))
      message("  actions:  ", paste_vector(self$project$action_ids))
      message("  geometry: ", inherits(self$project$site_geometry, "sf"))
      invisible(self)
    },

    #' @description
    #' Generate a `character` summarizing the representation of the object.
    #' @return `character` value.
    repr = function() {
      paste0("Solution (", self$id, ")")
    },

    #' @description
    #' Get site identifiers.
    #' @return `character` vector.
    get_site_ids = function() {
      self$project$site_ids
    },

    #' @description
    #' Get feature identifiers.
    #' @return `character` vector.
    get_feature_ids = function() {
      self$project$feature_ids

    },

    #' @description
    #' Get action identifiers.
    #' @return `character` vector.
    get_action_ids = function() {
      self$project$action_ids
    },

    #' @description
    #' Display summary results data using \pkg{rhandontable}.
    render_summary_results = function() {
      is_error <- identical(
        self$summary_results_data[[1]][[1]],
        self$project$parameters$error_sheets$main_message
      )
      if (is_error) {
        # use defaults if not showing specific error message
        out <-
          rhandsontable::hot_cols(
            rhandsontable::rhandsontable(self$summary_results, useTypes = TRUE),
            readOnly = TRUE
          )
      } else {
        # manually specify column width
        out <-
          rhandsontable::hot_cols(
            rhandsontable::rhandsontable(self$summary_results, useTypes = TRUE),
            readOnly = TRUE, colWidths = rep(300, ncol(self$summary_results))
          )
      }
      out
    },

    #' @description
    #' Display site results data using \pkg{rhandontable}.
    render_site_results = function() {
      rhandsontable::hot_cols(
        rhandsontable::rhandsontable(self$site_results, useTypes = TRUE),
        readOnly = TRUE,
        colWidths = c(300, rep(200, ncol(self$site_results) - 1))
      )
    },

    #' @description
    #' Display feature results data using \pkg{rhandontable}.
    render_feature_results = function() {
      rhandsontable::hot_cols(
        rhandsontable::rhandsontable(self$feature_results, useTypes = TRUE),
        readOnly = TRUE,
        colWidths = c(300, rep(200, ncol(self$feature_results) - 1))
      )
    },

    #' @description
    #' Get the bounding box.
    #' @param native `logical` indicating if the bounding box should
    #'   be in (`TRUE`) the native coordinate reference system or (`FALSE`)
    #'   re-projected to longitude/latitude?
    #' @param expand `FALSE` should the bounding box be expanded by 10%?
    #' @return `list` object with `"xmin"`, `"xmax"`, `"ymin"`, and `"ymax"`
    #'   elements.
    get_bbox = function(native = TRUE, expand = FALSE) {
      self$project$get_bbox(native = native, expand = expand)
    },

    #' @description
    #' Write the data to disk.
    #' @param workbook_path `character` file path.
    #' @param geometry_path `character` file path.
    write = function(workbook_path, geometry_path) {
      # assert that arguments are valid
      assertthat::assert_that(
        assertthat::is.string(workbook_path),
        assertthat::noNA(workbook_path),
        assertthat::is.string(geometry_path),
        assertthat::noNA(geometry_path)
      )
      # workbook
      ## create workbook
      openxlsx::saveWorkbook(
        whatdataio::create_solution_workbook(
          ## variables
          site_ids = self$project$site_ids,
          feature_ids = self$project$feature_ids,
          action_ids = self$project$action_ids,
          site_descriptions = self$project$site_descriptions,
          feature_descriptions = self$project$feature_descriptions,
          action_descriptions = self$project$action_descriptions,
          ## data
          site_data = self$project$site_data,
          feasibility_data = self$project$feasibility_data,
          feature_data = self$project$feature_data,
          action_expectation_data = self$project$action_expectation_data,
          ## data comments
          site_comments = whatdataio::template_site_comments(
            site_descriptions = self$project$site_descriptions,
            action_descriptions = self$project$action_descriptions,
            parameters = self$project$parameters
          ),
          feasibility_comments = whatdataio::template_feasibility_comments(
            site_descriptions = self$project$site_descriptions,
            action_descriptions = self$project$action_descriptions,
            parameters = self$project$parameters
          ),
          feature_comments = whatdataio::template_feature_comments(
            feature_descriptions = self$project$feature_descriptions,
            parameters = self$project$parameters
          ),
          action_expectation_comments = lapply(
            self$action_ids, function(i) {
              whatdataio::template_action_expectation_comments(
                site_descriptions = self$project$site_descriptions,
                feature_descriptions = self$project$feature_descriptions,
                action_id = i,
                parameters = self$project$parameters
              )
            }
          ),
          ## parameters
          parameters = self$project$parameters
        ),
        returnValue = FALSE
      )
      # geometry data
      if (inherits(self$project$geometry_data, "sf")) {
        suppressWarnings({
          sf::write_sf(self$project$geometry_data, spatial_path)
        })
      }
      # return result
      invisible(self)
    }

  )
)

#' New solution
#'
#' Create a new [Solution] object.
#'
#' @param project [Project] object.
#'
#' @param summary_results `data.frame` containing the summary results.
#'
#' @param site_results `data.frame` containing the site results.
#'
#' @param feature_results `data.frame` containing the feature results.
#'
#' @inheritParams new_project
#'
#' @return A [Solution] object.
#'
#' @examples
#' #TODO
#' @export
new_solution <- function(project,
                         summary_results,
                         site_results,
                         feature_results,
                         id = uuid::UUIDgenerate()) {
  # create new dataset
  Solution$new(
    id = id,
    project = project,
    site_results = site_results,
    feature_results = feature_results
  )
}
