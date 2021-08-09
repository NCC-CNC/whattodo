#' Sever function: update map
#'
#' Set behavior for map.
#'
#' @details
#' This object is designed to be used within [app_server] function.
#' Within the [app_server] function, it should be called like this:
#'
#' ```
#' eval(server_map)
#' ```
#'
#' @noRd
server_update_map <- quote({

  # leaflet map
  shiny::observe({
    if (!is.null(values$site_spatial_data) &&
      !identical(
        values$site_results_data,
        default_tabular_data()
      )
    ) {
      ### if results have updated then...
      if (!identical(
        names(values$site_results_data)[[1]],
        parameters$error_sheets$name_header
      )) {
        ## update map with solution if valid result
        update_map(
          leaflet::leafletProxy("map_widget", session),
          feature_names = values$feature_names,
          site_spatial_data = values$site_spatial_data,
          site_results_data = values$site_results_data,
          action_expectation_data = values$action_expectation_data,
          action_colors = values$action_colors,
          legend_title = "Priority actions",
          parameters = parameters
        )
      } else {
        ## update map with no solution if invalid
        reset_map(
          leaflet::leafletProxy("map_widget", session),
          site_spatial_data = values$site_spatial_data,
          legend_title = NULL,
          parameters = parameters
        )
      }
    }
  })

})
