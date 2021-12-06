#' Sever function: update data
#'
#' Set behavior for updating data.
#'
#' @details
#' This object is designed to be used within [app_server] function.
#' Within the [app_server] function, it should be called like this:
#'
#' ```
#' eval(server_update_data)
#' ```
#'
#' @noRd
server_update_data <- quote({

  # project site data
  shiny::observeEvent(input$site_data_widget, {
    shiny::req(input$site_data_widget)
    app_data$project$set_site_data(
      rhandsontable::hot_to_r(site_data_widget)
    )
  })

  # project feature data
  shiny::observeEvent(input$feature_data_widget, {
    shiny::req(input$feature_data_widget)
    app_data$project$set_feature_data(
      rhandsontable::hot_to_r(feature_data_widget)
    )
  })

  # project feasibility data
  shiny::observeEvent(input$feasibility_data_widget, {
    shiny::req(input$feasibility_data_widget)
    app_data$project$set_feasibility_data(
      rhandsontable::hot_to_r(feasibility_data_widget)
    )
  })

  # project action expectation data
  for (i in seq_along(app_data$project$get_action_ids())) {
    shiny::observeEvent(input[[paste0("action_", i, "_widget")]], {
      x <- input$input[[paste0("action_", i, "_widget")]]
      shiny::req(x)
      app_data$project$set_action_expectation_data(
        rhandsontable::hot_to_r(x),
        action_id = app_data$project$get_action_ids()[[i]]
      )
    })
  }

})
