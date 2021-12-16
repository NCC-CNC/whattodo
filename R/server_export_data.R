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
  output$exportPane_button <- downloadHandler(
    filename = function() {
      ## create default filename based on download option
      if (identical(input$exportPane_select, app_data$project_data_id)) {
        out <- paste0("project-data", Sys.Date(), ".zip")
      } else {
        proj <- which(input$exportPane_select == app_data$solution_ids)
        proj <- names(app_data$solution_ids[proj])
        out <- paste0("solution-", proj, "-", Sys.Date(), ".zip")
      }
      out
    },
    content = function(con) {
      ## create file names
      d <- tempfile()
      dir.create(d, showWarnings = FALSE, recursive = TRUE)
      ## save files to disk
      if (identical(input$exportPane_select, app_data$project_data_id)) {
        app_data$project$write(
          file.path(d, "project.xlsx"),
          file.path(d, "project.shp")
        )
      } else {
        ## find solution
        i <- which(input$exportPane_select == app_data$solution_ids)
        app_data$solution[[i]]$write(
          file.path(d, "solution.xlsx"),
          file.path(d, "solution.shp")
        )
      }
      ## create zip file with all files
      withr::with_dir(d, {
        utils::zip(zipfile = con, files = dir(), flags = c("-r9X", "-qq"))
      })
      ## clean up
      unlink(d, force = FALSE, recursive = TRUE)
    }
  )
})
