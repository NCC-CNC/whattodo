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
    new_setting <- input$newSolutionPane_settings$setting

    ## update setting
    if (identical(new_setting$setting, "feature_goal")) {
      project$set_feature_goal(
        feature_id = new_setting$id,
        value = new_setting$value
      )
    } else if (identical(new_setting$setting, "feature_weight")) {
      project$set_feature_weight(
        feature_id = new_setting$id,
        value = new_setting$value
      )
    } else {
      warning(
        paste0("setting not recognized: ", new_setting$setting),
        immediate. = TRUE
      )
    }

  })

})
