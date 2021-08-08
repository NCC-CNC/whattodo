#' Sever function: initialize application
#'
#' Set behavior for initializing the application.
#'
#' @details
#' This object is designed to be used within [app_server] function.
#' Within the [app_server] function, it should be called like this:
#'
#' ```
#' eval(server_initialize_app)
#' ```
#'
#' @noRd
server_initialize_app <- quote({

  # initialize data
  values <- shiny::reactiveValues(
    ## app state variables
    spreadsheet_data_present = FALSE,
    shapefile_data_present = FALSE,
    shapefile_data_required = FALSE,
    data_ready = FALSE,
    ## data variables
    site_names = character(0),
    feature_names = character(0),
    action_names = character(0),
    action_colors = NULL,
    ## spatial data
    site_spatial_data = NULL,
    site_bbox = actionmisc::default_bbox(),
    ## data tables
    site_data = actionmisc::default_tabular_data(),
    site_status_data = actionmisc::default_tabular_data(),
    feature_data = actionmisc::default_tabular_data(),
    action_expectation_data = list(actionmisc::default_tabular_data()),
    ## results tables
    summary_results_data = actionmisc::default_tabular_data(),
    site_results_data = actionmisc::default_tabular_data(),
    feature_results_data = actionmisc::default_tabular_data(),
    ## ui components
    data_ui_data = actionmisc::default_data_ui(),
    results_ui_data = actionmisc::default_results_ui(),
    sidebar_ui_data = actionmisc::default_sidebar_ui(),
    map_ui_data = actionmisc::default_map_ui()
  )

  # user interfaces
  output$map_ui <- shiny::renderUI(values[["map_ui_data"]])
  output$sidebar_ui <- shiny::renderUI(values[["sidebar_ui_data"]])
  output$data_ui <- shiny::renderUI(values[["data_ui_data"]])
  output$results_ui <- shiny::renderUI(values[["results_ui_data"]])

  # disable buttons
  shinyjs::disable("data_done_btn")

  # data tables
  output$site_data_widget <- rhandsontable::renderRHandsontable({
    actionmisc::render_site_data(default_tabular_data())
  })
  output$site_status_widget <- rhandsontable::renderRHandsontable({
    actionmisc::render_site_data(default_tabular_data())
  })
  output$feature_data_widget <- rhandsontable::renderRHandsontable({
    actionmisc::render_site_data(default_tabular_data())
  })

  # results tables
  output$summary_results_widget <- rhandsontable::renderRHandsontable({
    rhandsontable::hot_cols(
      rhandsontable::rhandsontable(
        actionmisc::default_tabular_data(),
        useTypes = TRUE
      ),
      readOnly = TRUE
    )
  })
  output$site_results_widget <- rhandsontable::renderRHandsontable({
    rhandsontable::holt_cols(
      rhandsontable::rhandsontable(
        actionmisc::default_tabular_data(),
        useTypes = TRUE
      ),
      readOnly = TRUE
    )
  })
  output$feature_results_widget <- rhandsontable::renderRHandsontable({
    rhandsontable::holt_cols(
      rhandsontable::rhandsontable(
        actionmisc::default_tabular_data(),
        useTypes = TRUE
      ),
      readOnly = TRUE
    )
  })

  # leaflet map
  shiny::observe({
    if (!is.null(values$site_bbox)) {
      output$map_widget <- leaflet::renderLeaflet({
        actionmisc::initialize_map(
          values$site_bbox,
          site_spatial_data = values$site_spatial_data,
          parameters
        )
      })
    }
  })

  # alert modal
  output$alert_modal_title <- shiny::renderText("Error")
  output$alert_modal_msg <- shiny::renderText("Something went horribly wrong!")

})
