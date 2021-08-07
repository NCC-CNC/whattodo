#' Sever function: generate prioritization
#'
#' Set behavior for generating prioritizations.
#'
#' @details
#' This object is designed to be used within [app_server] function.
#' Within the [app_server] function, it should be called like this:
#'
#' ```
#' eval(server_generate_prioritization)
#' ```
#'
#' @noRd
server_generate_prioritization <- quote({

  # observer for generating prioritizations
  shiny::observeEvent(input$solution_btn, {
    # disable button while running
    shinyjs::disable("solution_btn")
    # generate prioritization
    if (input$problem_widget < 0.5) {
      solution <- actionmisc::prioritization_without_budget(
        site_names = values$site_names,
        action_names = values$action_names,
        feature_names = values$feature_names,
        site_data = values$site_data,
        site_status_data = values$site_status_data,
        feature_data = values$feature_data,
        action_expectation_data = values$action_expectation_data,
        gap = 0,
        parameters = parameters
      )
    } else {
      solution <- actionmisc::prioritization_with_budget(
        site_names = values$site_names,
        action_names = values$action_names,
        feature_names = values$feature_names,
        site_data = values$site_data,
        site_status_data = values$site_status_data,
        feature_data = values$feature_data,
        action_expectation_data = values$action_expectation_data,
        budget = input$budget_widget,
        weights = TRUE,
        gap = 0,
        parameters = parameters
      )
    }

    # update internal variables
    values[["summary_results_data"]] <- solution$summary_results
    values[["site_results_data"]] <- solution$site_results
    values[["feature_results_data"]] <- solution$feature_results

    # update widgets with results from prioritization
    output$summary_results_widget <- rhandsontable::renderRHandsontable({
      actionmisc::render_summary_results_data(
        values[["summary_results_data"]],
        parameters
      )
    })
    output$site_results_widget <- rhandsontable::renderRHandsontable({
      rhandsontable::holt_cols(
        rhandsontable::rhandsontable(
          values[["site_results_data"]],
          useTypes = TRUE
        ),
        readOnly = TRUE
      )
    })
    output$feature_results_widget <- rhandsontable::renderRHandsontable({
      actionmisc::render_feature_results_data(
        values[["feature_results_data"]],
        parameters
      )
    })

    # check if solution was obtained
    if (!isTRUE(solution$solved)) {
      # display error notification if couldn't find solution
      ## display alert
      output$alert_modal_title <-
        shiny::renderText("Oops: infeasible problem")
      output$alert_modal_msg <-
        shiny::renderText(paste(
          "There is no possible solution to the specified problem.",
          "Please see the \"Summary results\" tab under the \"Results\" tab",
          "for possible explanations."))
      shinyBS::toggleModal(session, "alert_modal", toggle = "open")
      shinyjs::disable("export_btn")
      shinyBS::addTooltip(
        session, "export_btn_div",
        "Please generate a valid prioritization to download the results."
      )
    } else {
      # enable download button
      shinyjs::enable("export_btn")
      try(shinyBS::removeTooltip(
        session,
        "export_btn_div"
      ), silent = TRUE)
    }

    # re-enable the button
    shinyjs::enable("solution_btn")
  })

})
