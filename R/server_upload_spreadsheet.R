#' Sever function: upload spreadsheet
#'
#' Set behavior for uploading spreadsheet.
#'
#' @details
#' This object is designed to be used within [app_server] function.
#' Within the [app_server] function, it should be called like this:
#'
#' ```
#' eval(server_upload_data)
#' ```
#'
#' @noRd
server_upload_data <- quote({

  # store input data if they have been updated
  shiny::observeEvent(input$site_data_widget, {
    if (values$data_ready) {
      if (!is.null(input$site_data_widget)) {
        values[["site_data"]] <-
          actionmisc::parse_site_data(input$site_data_widget)
      }
    }
  })
  shiny::observeEvent(input$site_status_widget, {
    if (values$data_ready) {
      if (!is.null(input$site_status_widget)) {
        values[["site_status_data"]] <-
          actionmisc::parse_site_status_data(input$site_status_widget)
      }
    }
  })
  shiny::observeEvent(input$feature_data_widget, {
    if (values$data_ready) {
      if (!is.null(input$feature_data_widget)) {
        values[["feature_data"]] <-
          actionmisc::parse_feature_data(input$feature_data_widget)
      }
    }
  })
  shiny::observe({
    if (values$data_ready) {
      for (i in seq_along(values$action_names)) {
        n <- paste0("action_", i, "_widget")
        if (!is.null(input[[n]])) {
          values[["action_expectation_data"]][[i]] <-
            actionmisc::parse_action_expectation_data(input[[n]])
        }
      }
    }
  })

  # observers for spreadsheet upload
  shiny::observeEvent(input$spreadsheet_upload_widget, {
    ## verify if data uploaded
    shiny::req(input$spreadsheet_upload_widget)
    f <- input$spreadsheet_upload_widget$datapath
    ## validate spreadsheet format
    if (!actionmisc::is_valid_spreadsheet(f, parameters)) {
      ### display alert
      output$alert_modal_title <-
        shiny::renderText("Oops: invalid Excel Spreadsheet")
      output$alert_modal_msg <-
        shiny::renderText(
          "This Excel Spreadsheet wasn't generated using the NCC Data App.")
      shinyBS::toggleModal(session, "alert_modal", toggle = "open")
      ### update app variables
      values$spreadsheet_data_present <- FALSE
      shinyjs::disable("data_done_btn")
      ## exit
      return()
    }
    ## read spreadsheet data
    d <- actionmisc::read_spreadsheet_data(f, parameters)
    ## validate spreadsheet data
    if (!actionmisc::has_valid_spreadsheet_data(d)) {
      ### display
      output$alert_modal_title <-
        shiny::renderText("Oops: Excel Spreadsheet missing data")
      output$alert_modal_msg <-
        shiny::renderText(paste(
          "This Excel Spreadsheet is missing data in required",
          "cells. Please note that all light grey",
          "cells should have a value in them (even if just a 0)."))
      shinyBS::toggleModal(session, "alert_modal", toggle = "open")
      ### update app variables
      values$spreadsheet_data_present <- FALSE
      shinyjs::disable("data_done_btn")
      ### exit
      return()
    }
    ## store data
    values$spreadsheet_data_present <- TRUE
    values[["site_names"]] <- d$site_names
    values[["feature_names"]] <- d$feature_names
    values[["action_names"]] <- d$action_names
    values[["site_data"]] <- d$site_data
    values[["site_status_data"]] <- d$site_status_data
    values[["feature_data"]] <- d$feature_data
    values[["action_expectation_data"]] <- d$action_expectation_data
    values[["action_colors"]] <-
      actionmisc::default_colors(d$action_names, parameters)
    ### enable data done button if relevant
    if (
      ((isTRUE(values$shapefile_data_present) &&
        isTRUE(values$shapefile_data_required)) ||
       (actionmisc::isFALSE(values$shapefile_data_required))) &&
      isTRUE(values$spreadsheet_data_present)) {
      shinyjs::enable("data_done_btn")
    }
  })


})
