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

  # update map based on home button
  shiny::observeEvent(input$home_button, {
    ## specify dependencies
    shiny::req(input$home_button)

    ## update map
    leaflet::flyToBounds(
      map = leaflet::leafletProxy("map"),
      lng1 = app_data$bbox$xmin,
      lat1 = app_data$bbox$ymin,
      lng2 = app_data$bbox$xmax,
      lat2 = app_data$bbox$ymax
    )
  })

  # update map based on print button
  shiny::observeEvent(input$print_button, {
    ## specify dependencies
    shiny::req(input$print_button)

    ## save screenshot of map
    leaflet.extras2::easyprintMap(
      map = leaflet::leafletProxy("map"),
      sizeModes = "A4Landscape",
      filename = "map"
    )
  })

  # update map layer control when dataset changes
  shiny::observeEvent(input$map_dataset, {
    shiny::req(input$map_dataset)
    if (identical(input$map_dataset, "NA")) {
      return()
    }
    ## get layer names
    if (identical(input$map_dataset, app_data$project_data_id)) {
      nm <- app_data$project$get_map_layers()
    } else {
      i <- which(input$map_dataset == app_data$solution_ids)
      nm <- app_data$solution[[i]]$get_map_layers()
    }
    ## see if layer name needs to change
    if (isTRUE(input$map_layer %in% nm)) {
      l <- input$map_layer
    } else {
      l <- nm[1]
    }
    ## update select input
    shiny::updateSelectInput(
      session = session,
      inputId = "map_layer",
      choices = nm,
      select = l
    )
  })

  # update layer shown on map when map layer changes
  shiny::observeEvent(input$map_layer, {
    shiny::req(input$map_dataset)
    shiny::req(input$map_layer)
    if (identical(input$map_dataset, "NA")) {
      return()
    }
    ## extract data to render on map
    if (identical(input$map_dataset, app_data$project_data_id)) {
      d <- app_data$project
    } else {
      i <- which(input$map_dataset == app_data$solution_ids)
      d <- app_data$solution[[i]]
    }
    ## render data on map
    d$render_on_map(
      map = leaflet::leafletProxy("map"),
      data = input$map_layer
    )
  })

})
