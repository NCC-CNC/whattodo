#' Sever function: upload shapefile
#'
#' Set behavior for uploading shpaefile.
#'
#' @details
#' This object is designed to be used within [app_server] function.
#' Within the [app_server] function, it should be called like this:
#'
#' ```
#' eval(server_upload_shapefile)
#' ```
#'
#' @noRd
server_upload_shapefile <- quote({

  # observe shapefile query widget to see if user wants to supply a shapefile
  shiny::observeEvent(input$shapefile_query_widget, {
    ## store checkbox result
    if (!is.null(input$shapefile_query_widget)) {
      values[["shapefile_data_required"]] <-
        as.logical(input$shapefile_query_widget)
      shinyjs::disable("data_done_btn")
    }
    ## update data done button if they decide they want shapefile
    if (isTRUE(values$shapefile_data_required) &&
      isFALSE(values$shapefile_data_present)) {
      shinyjs::disable("data_done_btn")
    }
    ## update data done button if they decide they don't want to
    ## bother with the shapefile
    if (
      ((isTRUE(values$shapefile_data_present) &&
        isTRUE(values$shapefile_data_required)) ||
        (isFALSE(values$shapefile_data_present) &&
          isFALSE(values$shapefile_data_required))) &&
        isTRUE(values$spreadsheet_data_present)) {
      shinyjs::enable("data_done_btn")
    }
  })

  # observers for shapefile upload
  shiny::observeEvent(input$shapefile_upload_widget, {
    ## verify if data uploaded
    shiny::req(input$shapefile_upload_widget)
    f <- input$shapefile_upload_widget$datapath[[1]]
    f_shp <- paste0(tools::file_path_sans_ext(f), ".shp")
    f2 <- input$shapefile_upload_widget$name[[1]]
    f2_sans_ext <- basename(tools::file_path_sans_ext(f2))
    ## validate single basename
    if (dplyr::n_distinct(basename(tools::file_path_sans_ext(
      input$shapefile_upload_widget$name
    ))) > 1) {
      ### display
      output$alert_modal_title <-
        shiny::renderText("Oops: multiple shapefiles specified")
      output$alert_modal_msg <-
        shiny::renderText(paste0(
          "It would seem that you have specified multiple shapefiles. ",
          "All of the underyling files that constitute a single shapefile ",
          "should begin with the same prefix ",
          "(e.g. \"data.shp\", \"data.prj\", \"data.shx\", and \"data.dbf\")."
        ))
      shinyBS::toggleModal(session, "alert_modal", toggle = "open")
      ### update app variables
      values$shapefile_data_present <- FALSE
      shinyjs::disable("data_done_btn")
      ### exit
      return()
    }
    ## rename files to conform to shapefile
    prepare_for_shapefile_import(
      input$shapefile_upload_widget$datapath
    )
    ## validate shapefile format
    if (!is_valid_shapefile(f_shp)) {
      ### display
      output$alert_modal_title <-
        shiny::renderText("Oops: invalid files")
      output$alert_modal_msg <-
        shiny::renderText(paste0(
          "This shapefile doesn't seem valid. ",
          "Please ensure that you have selected all of these files when ",
          "uploading the data: ",
          "\"", f2_sans_ext, ".dbf\", \"", f2_sans_ext, ".shp\", \"",
          f2_sans_ext, ".shx\", and \"", f2_sans_ext, ".prj\"."
        ))
      shinyBS::toggleModal(session, "alert_modal", toggle = "open")
      ### update app variables
      values$shapefile_data_present <- FALSE
      shinyjs::disable("data_done_btn")
      ### exit
      return()
    }
    ## import shapefile
    d <- sf::read_sf(f_shp)
    ## verify has "name" column
    if (!"name" %in% names(d)) {
      ### display alert
      output$alert_modal_title <-
        shiny::renderText("Oops: shapefile missing field")
      output$alert_modal_msg <-
        shiny::renderText(paste0(
          "Please ensure that this shapefile has a \"name\" field ",
          " in the attribute table that contains the name of each site ",
          "(i.e. values in the \"",
          parameters$site_data_sheet$name_header,
          "\" column of the \"",
          parameters$site_data_sheet$sheet_name,
          "\" sheet in the Excel Spreadsheet). ",
          "To address this, you will need to update the shapefile using a ",
          "geographic information system (GIS), such as ArcGIS, QGIS, or R, ",
          "and try uploading the shapefile again."
        ))
      shinyBS::toggleModal(session, "alert_modal", toggle = "open")
      ### update app variables
      values$shapefile_data_present <- FALSE
      shinyjs::disable("data_done_btn")
      ### exit
      return()
    }
    ## store data
    values$shapefile_data_present <- TRUE
    values[["site_spatial_data"]] <- d
    ### enable data done button if relevant
    if (
      ((isTRUE(values$shapefile_data_present) &&
        isTRUE(values$shapefile_data_required)) ||
        (isFALSE(values$shapefile_data_present) &&
          isFALSE(values$shapefile_data_required))) &&
        isTRUE(values$spreadsheet_data_present)) {
      shinyjs::enable("data_done_btn")
    }
  })

})
