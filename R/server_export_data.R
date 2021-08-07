#' Sever function: export data
#'
#' Set behavior for exporting data.
#'
#' @details
#' This object is designed to be used within [app_server] function.
#' Within the [app_server] function, it should be called like this:
#'
#' ```
#' eval(server_export_data)
#' ```
#'
#' @noRd
server_export_data <- quote({
  # observer for exporting results
  output$export_btn <- downloadHandler(
    filename = function() {
      paste0("prioritization-", Sys.Date(), ".xlsx")
    },
    content = function(con) {
      # create workbook
      w <- actionmisc::create_export_workbook(
        site_names = values$site_names,
        feature_names = values$feature_names,
        action_names = values$action_names,
        site_data = values$site_data,
        site_status_data = values$site_status_data,
        feature_data = values$feature_data,
        action_expectation_data = values$action_expectation_data,
        summary_results_data = values$summary_results_data,
        site_results_data = values$site_results_data,
        feature_results_data = values$feature_results_data,
        parameters = parameters
      )
      # save workbook to disk
      openxlsx::saveWorkbook(w, con, returnValue = FALSE)
    }
  )
})
