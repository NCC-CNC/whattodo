#' Sever function: update solution settings
#'
#' Set behavior for updating the solution settings sidebar content.
#'
#' @details
#' This object is designed to be used within [app_server] function.
#' Within the [app_server] function, it should be called like this:
#'
#' ```
#' eval(server_update_solution_settings)
#' ```
#'
#' @noRd
server_update_solution_settings <- quote({

  # update solution settings
  shiny::observeEvent(input$newSolutionPane_settings, {
    ## specify dependencies
    shiny::req(input$newSolutionPane_settings)

    ## extract new setting
    new_setting <- input$newSolutionPane_settings

    ## update settings
    if (identical(new_setting$type, "parameter")) {
      ### update budget setting
      app_data$project$settings[[1]]$set_setting(
        name = new_setting$setting,
        value = new_setting$value
      )
    } else {
      ### extract feature id
      id <- app_data$project$get_feature_ids_from_html_id(
        substring(new_setting$id, 2)
      )
      ### update feature setting
      if (identical(new_setting$type, "theme")) {
        app_data$project$set_feature_goal(
          feature_id = id,
          value = new_setting$value * 100
        )
      } else if (identical(new_setting$type, "weight")) {
        app_data$project$set_feature_weight(
          feature_id = id,
          value = new_setting$value
        )
      } else {
        warning(
          paste0("setting not recognized: ", new_setting$setting),
          immediate. = TRUE
        )
      }
    }

  })

})
