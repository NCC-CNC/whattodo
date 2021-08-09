#' Sever function: import data
#'
#' Set behavior for importing data.
#'
#' @details
#' This object is designed to be used within [app_server] function.
#' Within the [app_server] function, it should be called like this:
#'
#' ```
#' eval(server_import_data)
#' ```
#'
#' @noRd
server_import_data <- quote({

  # observe data done button
  shiny::observeEvent(input$data_done_btn, {
    # verify shapefile is correct if specified
    if (isTRUE(values[["shapefile_data_required"]])) {
      ## verify that shapefile and site data have same number of sites
      if (!identical(
        length(values$site_names),
        length(values$site_spatial_data$name)
      )) {
        ### display alert
        output$alert_modal_title <-
          shiny::renderText("Oops: invalid shapefile")
        output$alert_modal_msg <-
          shiny::renderText(paste0(
            "Please ensure that the shapefile has the same number of ",
            "sites (rows in the attribute table) as the Excel Spreadsheet ",
            "data (i.e. number of rows in the \"",
            parameters$site_data_sheet$sheet_name, "\" sheet). ",
            "To address this, you will need to update the shapefile using a ",
            "geographic information system (GIS), such as ArcGIS, QGIS, or R, ",
            "and try uploading the shapefile again."
          ))
        shinyBS::toggleModal(session, "alert_modal", toggle = "open")
        ### update app variables
        values$shapefile_data_present <- FALSE
        values[["site_spatial_data"]] <- NULL
        shinyjs::disable("data_done_btn")
        ### exit
        return()
      }
      ## verify that shapefile and site data have same site names
      if (!identical(
        sort(values$site_names), sort(values$site_spatial_data$name)
      )) {
        ### display alert
        msg_names <-
          incorrect_names(
            values$site_names,
            values$site_spatial_data$name
          )
        output$alert_modal_title <-
          shiny::renderText("Oops: invalid shapefile")
        output$alert_modal_msg <-
          shiny::renderText(paste0(
            "Please ensure that the site names in the shapefile ",
            "(i.e. values in the \"name\" field of the attribute table) ",
            "have exactly the same values as those in the Excel Spreadsheet ",
            "(i.e. values in the \"",
            parameters$site_data_sheet$name_header,
            "\" column of the \"",
            parameters$site_data_sheet$sheet_name,
            "\" sheet). ",
            msg_names,
            "To address this, you will need to update the shapefile using a ",
            "geographic information system (GIS), such as ArcGIS, QGIS, or R, ",
            "and try uploading the shapefile again."
          ))
        shinyBS::toggleModal(session, "alert_modal", toggle = "open")
        ### update app variables
        values$shapefile_data_present <- FALSE
        values[["site_spatial_data"]] <- NULL
        shinyjs::disable("data_done_btn")
        ### exit
        return()
      }
    }
    ## update app state variable
    values$data_ready <- TRUE
    ## if no spatial data supplied, then it using lon/lat columns
    if (is.null(values[["site_spatial_data"]])) {
      site_data <- values[["site_data"]]
      values[["site_spatial_data"]] <-
        sf::st_as_sf(
          tibble::tibble(
            name = site_data[[parameters$site_data_sheet$name_header]],
            x = site_data[[parameters$site_data_sheet$longitude_header]],
            y = site_data[[parameters$site_data_sheet$latitude_header]]
          ),
          coords = c("x", "y"),
          crs = 4326
        )
    }
    ## reorder site spatial data to match site data
    idx <- match(
      values[["site_spatial_data"]]$name,
      values[["site_data"]][[parameters$site_data_sheet$name_header]]
    )
    assertthat::assert_that(
      assertthat::noNA(idx),
      msg = "shaprfile and Excel spreadsheet have different names "
    )
    values[["site_spatial_data"]] <-
      values[["site_spatial_data"]][idx, , drop = FALSE]

    ## add id column to spatial data
    values[["site_spatial_data"]]$id <-
      paste0("F", seq_len(nrow(values[["site_data"]])))
    ## reproject spatial data to WGS1984 for leaflet
    ## see https://rstudio.github.io/leaflet/projections.html
    values[["site_spatial_data"]] <-
      sf::st_transform(values[["site_spatial_data"]], 4326)
    ## compute bounding box for map
    values[["site_bbox"]] <-
      sf::st_bbox(values[["site_spatial_data"]])
    ## switch app from default state to primary state
    values[["data_ui_data"]] <- primary_data_ui(
      site_names = values$site_names,
      feature_names = values$feature_names,
      action_names = values$action_names,
      parameters = parameters
    )
    values[["results_ui_data"]] <- primary_results_ui(
      site_names = values$site_names,
      feature_names = values$feature_names,
      action_names = values$action_names,
      parameters = parameters
    )
    values[["sidebar_ui_data"]] <- primary_sidebar_ui(
      site_names = values$site_names,
      feature_names = values$feature_names,
      action_names = values$action_names,
      parameters = parameters
    )
    values[["map_ui_data"]] <- primary_map_ui(
      site_names = values$site_names,
      feature_names = values$feature_names,
      action_names = values$action_names,
      parameters = parameters
    )
    ## update tables
    output$site_data_widget <- rhandsontable::renderRHandsontable({
      render_site_data(
        values[["site_data"]]
      )
    })
    output$site_status_widget <- rhandsontable::renderRHandsontable({
      render_site_status_data(
        values[["site_status_data"]],
        parameters
      )
    })
    output$feature_data_widget <- rhandsontable::renderRHandsontable({
      render_feature_data(
        values[["feature_data"]]
      )
    })
    lapply(seq_along(values$action_names), function(i) {
      output[[paste0("action_", i, "_widget")]] <-
        rhandsontable::renderRHandsontable(
          render_action_expectation_data(
            values[["action_expectation_data"]][[paste0("action_", i)]]
          )
        )
    })
  })

})
