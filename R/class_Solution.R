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

    #' @field name `character` name.
    name = NA_character_,

    #' @field project [Project] object.
    project = NULL,

    #' @field settings `list` of [Parameter] objects.
    settings = NULL,

    #' @field summary_results `data.frame` object.
    summary_results = NULL,

    #' @field site_results `data.frame` object.
    site_results = NULL,

    #' @field feature_results `data.frame` object.
    feature_results = NULL,

    #' @description
    #' Create a Solution object.
    #' @param id `character` identifier.
    #' @param name `character` value.
    #' @param project `character` identifier.
    #' @param settings `list` of [Parameter] objects.
    #' @param summary_results `data.frame` object.
    #' @param site_results `data.frame` object.
    #' @param feature_results `data.frame` object.
    #' @return A new Solution object.
    initialize = function(id,
                          name,
                          project,
                          settings,
                          summary_results,
                          site_results,
                          feature_results) {
      # assert that arguments are valid
      assertthat::assert_that(
        assertthat::is.string(id),
        assertthat::noNA(id),
        assertthat::is.string(name),
        assertthat::noNA(name),
        is.list(settings),
        all_list_elements_inherit(settings, "Parameter"),
        inherits(project, "Project"),
        inherits(summary_results, "data.frame"),
        inherits(site_results, "data.frame"),
        inherits(feature_results, "data.frame")
      )

      # assign fields
      self$id <- id
      self$name <- name
      self$project <- project
      self$settings <- settings
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
      message("  name:     ", self$name)
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
    #' Get layer names for rendering data on map
    get_map_layers = function() {
      c(
        "Priority actions" = "priority_actions",
        self$project$get_map_layers()
      )
    },

    #' @description
    #' Get the bounding box.
    #' @param expand `FALSE` should the bounding box be expanded by 10%?
    #' @return `list` object with `"xmin"`, `"xmax"`, `"ymin"`, and `"ymax"`
    #'   elements.
    get_bbox = function(expand = FALSE) {
      self$project$get_bbox(expand = expand)
    },

    #' @description
    #' Render on map.
    #' @param map [leaflet::leaflet()] object.
    #' @param data `character` name of dataset to show.
    #'   Argument must be a valid layer name (see `self$get_map_layers()`).
    #' @param group `character` group name. Defaults to `"sites"`.
    #' @return [leaflet::leaflet()] map.
    render_on_map = function(map, data = "location", group = "sites") {
      # assert that argument is valid
      assertthat::assert_that(inherits(map, c("leaflet", "leaflet_proxy")))
      # update map
      if (!identical(data, "priority_actions")) {
        ## update with project data
        map <- self$project$render_on_map(
          map = map,
          data = data,
          group = group
        )
      } else {
        ## update with solution
        #### prepare data for map
        pal <- leaflet::colorFactor(
          palette = self$project$action_colors,
          domain = self$project$action_ids,
        )
        popups <- self$site_results
        vals <- popups[[2]]
        ### clear group from map
        map <- leaflet::clearGroup(map, group)
        map <- leaflet::clearShapes(map)
        map <- leaflet::removeControl(map, "legend")
        ### add data to map
        map <- leafem::addFeatures(
          map = map,
          data = self$project$site_geometry,
          groupId = group,
          color = pal(vals),
          fillColor = pal(vals),
          popup = leafpop::popupTable(
            x = popups,
            row.numbers = FALSE,
            feature.id = FALSE
          )
        )
        ### add legend to map
        map <- leaflet::addLegend(
          layerId = "legend",
          map = map,
          pal = pal,
          values = vals,
          position = "bottomright",
        )
      }
      # return result
      map
    },

    #' @description
    #' Get data for rendering widget to display results.
    get_solution_results_data = function() {
      # extract variables
      nh <- self$project$parameters$feature_data_sheet$name_header
      th <- self$project$parameters$feature_data_sheet$goal_header
      tah <- self$project$parameters$feature_results_sheet$total_amount_header
      # return data
      list(
        id = self$id,
        name = "solution",
        parameters = lapply(self$settings, function(x) x$get_widget_data()),
        statistics = lapply(seq_len(nrow(self$summary_results)), function(i) {
          list(
            name = self$summary_results[[1]][[i]],
            value = self$summary_results[[2]][[i]],
            units = dplyr::case_when(
              endsWith(self$summary_results[[1]][[i]], "cost") ~ "CAD",
              startsWith(self$summary_results[[1]][[i]], "Number") ~ "sites",
              TRUE ~ ""
            ),
            proportion = ""
          )
        }),
        theme_results = lapply(
          seq_len(nrow(self$feature_results)), function(i) {
            ## extract values
            max_held <- self$project$get_max_feature_consequence()$amount
            curr_held <- self$project$get_current_feature_consequence()$amount
            goal_held <- self$project$feature_data[[th]][[i]] / 100
            sol_held <- self$feature_results[[tah]]
            ## return result
            list(
              id = paste0("T", self$project$feature_html_ids[[i]]),
              name = self$project$feature_ids[[i]],
              feature_id = paste0("F", self$project$feature_html_ids[[i]]),
              feature_name = self$project$feature_ids[[i]],
              feature_status = isTRUE(
                self$project$feature_data[[th]][[i]] > 1e-10
              ),
              feature_total_amount = max_held[[i]],
              feature_current_held = curr_held[[i]] / max_held[[i]],
              feature_goal = goal_held,
              feature_solution_held = sol_held[[i]] / max_held[[i]],
              units = "units"
            )
          }
        ),
        solution_color = "#FF0000"
      )
    },

    #' @description
    #' Render site data.
    #' @param height `character`/`numeric` CSS measurement value.
    #'  Default to `"100%"`.
    #' @param width `character`/`numeric` CSS measurement value.
    #'  Default to `"100%"`.
    #' @return [rhandsontable::rhandsontable] object.
    render_site_data = function(height = "100%", width = "100%") {
      rhandsontable::hot_cols(
        self$project$render_site_data(
          height = height,
          width = width
        ),
        readOnly = TRUE
      )
    },

    #' @description
    #' Render feature data.
    #' @param height `character`/`numeric` CSS measurement value.
    #'  Default to `"100%"`.
    #' @param width `character`/`numeric` CSS measurement value.
    #'  Default to `"100%"`.
    #' @return [rhandsontable::rhandsontable] object.
    render_feature_data = function(height = "100%", width = "100%") {
      rhandsontable::hot_cols(
        self$project$render_feature_data(
          height = height,
          width = width
        ),
        readOnly = TRUE
      )
    },

    #' @description
    #' Render feasibility data.
    #' @param height `character`/`numeric` CSS measurement value.
    #'  Default to `"100%"`.
    #' @param width `character`/`numeric` CSS measurement value.
    #'  Default to `"100%"`.
    #' @return [rhandsontable::rhandsontable] object.
    render_feasibility_data = function(height = "100%", width = "100%") {
      rhandsontable::hot_cols(
        self$project$render_feasibility_data(
          height = height,
          width = width
        ),
        readOnly = TRUE
      )
    },

    #' @description
    #' Render consequence data.
    #' @param action_id `character` identifier for action.
    #' @param height `character`/`numeric` CSS measurement value.
    #'  Default to `"100%"`.
    #' @param width `character`/`numeric` CSS measurement value.
    #'  Default to `"100%"`.
    #' @return [rhandsontable::rhandsontable] object.
    render_consequence_data = function(action_id,
                                       height = "100%",
                                       width = "100%") {
      rhandsontable::hot_cols(
        self$project$render_consequence_data(
          action_id = action_id,
          height = height,
          width = width
        ),
        readOnly = TRUE
      )
    },

    #' @description
    #' Display summary results data.
    #' @return [DT::datatable()] object.
    render_summary_results = function() {
      d <- self$summary_results
      DT::datatable(
        d,
        rownames = FALSE,
        escape = FALSE,
        editable = FALSE,
        selection = "none",
        fillContainer = TRUE,
        options = list(
          ### align columns
          columnDefs = list(
            list(className = "dt-left", targets = 0),
            list(className = "dt-center", targets = seq(1, ncol(d) - 1))
          ),
          ### disable paging
          paging = FALSE,
          scrollCollapse = TRUE
        )
      ) %>%
      DT::formatRound(2, 2)
    },

    #' @description
    #' Display site results data.
    #' @return [DT::datatable()] object.
    render_site_results = function() {
      d <- self$site_results
      DT::datatable(
        d,
        rownames = FALSE,
        escape = FALSE,
        editable = FALSE,
        selection = "none",
        fillContainer = TRUE,
        options = list(
          ### align columns
          columnDefs = list(
            list(className = "dt-left", targets = 0),
            list(className = "dt-center", targets = seq(1, ncol(d) - 1))
          ),
          ### disable paging
          paging = FALSE,
          scrollCollapse = TRUE
        )
      )
    },

    #' @description
    #' Display feature results data.
    #' @return [DT::datatable()] object.
    render_feature_results = function() {
      d <- self$feature_results
      DT::datatable(
        d,
        rownames = FALSE,
        escape = FALSE,
        editable = FALSE,
        selection = "none",
        fillContainer = TRUE,
        options = list(
          ### align columns
          columnDefs = list(
            list(className = "dt-left", targets = 0),
            list(className = "dt-center", targets = seq(1, ncol(d) - 1))
          ),
          ### disable paging
          paging = FALSE,
          scrollCollapse = TRUE
        )
      ) %>%
      DT::formatRound(seq(2, ncol(d)), 2)
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
          consequence_data = self$project$consequence_data,
          ## results
          summary_results_data = self$summary_results,
          site_results_data = self$site_results,
          feature_results_data = self$feature_results,
          ## parameters
          parameters = self$project$parameters
        ),
        file = workbook_path,
        overwrite = TRUE,
        returnValue = FALSE
      )
      # geometry data
      if (inherits(self$project$site_geometry, "sf")) {
        suppressWarnings({
          sf::write_sf(self$project$site_geometry, geometry_path)
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
#' @param name `character` value with name for solution.
#'
#' @param settings `list` of [Parameter] objects.
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
                         name,
                         settings,
                         summary_results,
                         site_results,
                         feature_results,
                         id = uuid::UUIDgenerate()) {
  # create new dataset
  Solution$new(
    id = id,
    name = name,
    project = project,
    settings = settings,
    summary_results = summary_results,
    site_results = site_results,
    feature_results = feature_results
  )
}
