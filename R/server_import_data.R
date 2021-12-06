#' Sever function: import data
#'
#' Set behavior for importing data.
#'
#' @details
#' This object is designed to be used within [app_server] function.
#' Within the [app_server] function, it should be called like this:
#'
#' ```
#' eval(server_import_data)
#' ```
#'
#' @noRd
server_import_data <- quote({

  # store variables
  app_data$project <- x$project
  app_data$bbox <- x$project$get_bbox(native = FALSE, expand = TRUE)

  # update new solution sidebar
  output$newSolutionPane_settings <-
    renderSolutionSettings(solutionSettings(app_data$project))

  # update map
  map <- leaflet::leafletProxy("map")
  leaflet::flyToBounds(
    map, app_data$bbox$xmin, app_data$bbox$ymin,
    app_data$bbox$xmax, app_data$bbox$ymax
  )
  leaflet::fitBounds(
    map, app_data$bbox$xmin, app_data$bbox$ymin,
    app_data$bbox$xmax, app_data$bbox$ymax
  )
  app_data$project$initialize_map(map)

  # update data tables
  output$site_data_widget <- rhandsontable::renderRHandsontable({
    app_data$project$render_site_data()
  })
  output$feature_data_widget <- rhandsontable::renderRHandsontable({
    app_data$project$render_feature_data()
  })
  output$feasibility_data_widget <- rhandsontable::renderRHandsontable({
    app_data$project$render_feasibility_data()
  })
  lapply(seq(app_data$project$get_action_ids()), function(i) {
    output[[paste0("action_", i, "_widget")]] <-
      app_data$project$render_action_expectation_data(
        action_id = app_data$project$get_action_ids()[[i]]
      )
  })

  # make sidebars visible
  shinyjs::runjs("$('#mainSidebar').css('display','block');")

  # open sidebars
  leaflet.extras2::openSidebar(
    map,
    id = "newSolutionPane", sidebar_id = "mainSidebar"
  )

  # remove startup mode
  ## this makes the buttons and scalebar visible
  shinyjs::runjs("document.body.classList.remove('startup');")

  # return success
  invisible(TRUE)
})
