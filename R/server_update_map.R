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
})
