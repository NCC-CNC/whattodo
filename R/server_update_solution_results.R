#' Sever function: update solution results
#'
#' Set behavior for updating the solution results sidebar content.
#'
#' @details
#' This object is designed to be used within [app_server] function.
#' Within the [app_server] function, it should be called like this:
#'
#' ```
#' eval(server_update_solution_results)
#' ```
#'
#' @noRd
server_update_solution_results <- quote({

  # update solution results sidebar content
  shiny::observeEvent(input$solutionResultsPane_results_select, {
    ## specify dependencies
    shiny::req(input$solutionResultsPane_results_select)
    if (
      !input$solutionResultsPane_results_select %in% app_data$solution_ids) {
      return()
    }
    ## show solution results
    showSolutionResults(
      session = session,
      inputId = "solutionResultsPane_results",
      value = input$solutionResultsPane_results_select
    )
  })

  # update tables in solution modal
  shiny::observeEvent(input$solutionModal_select, {
    ## specify dependencies
    if (!input$solutionModal_select %in% app_data$solution_ids) {
      return()
    }
    ## find selected solution
    i <- unname(which(app_data$solution_ids == input$solutionModal_select))
    ## render data tables
    output$solution_data_modal_site_table <-
      rhandsontable::renderRHandsontable({
        app_data$solution[[i]]$render_site_data()
      })
    output$solution_data_modal_feasibility_table <-
      rhandsontable::renderRHandsontable({
        app_data$solution[[i]]$render_feasibility_data()
      })
    output$solution_data_modal_feature_table <-
      rhandsontable::renderRHandsontable({
        app_data$solution[[i]]$render_feature_data()
      })
    for (j in seq_along(app_data$project$action_ids)) {
      output[[paste0("solution_data_action_", j, "_table")]] <-
        rhandsontable::renderRHandsontable({
          app_data$solution[[i]]$render_action_expectation_data(
            app_data$project$action_ids[[j]]
          )
        })
    }
    ## render results tables
    output$solution_results_modal_summary_table <- DT::renderDT({
      app_data$solution[[i]]$render_summary_results()
    })
    output$solution_results_modal_site_table <- DT::renderDT({
      app_data$solution[[i]]$render_site_results()
    })
    output$solution_results_modal_feature_table <- DT::renderDT({
      app_data$solution[[i]]$render_feature_results()
    })

  })
})
