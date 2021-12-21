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

})
