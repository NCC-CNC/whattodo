#' Import data
#'
#' Import project data into the app.
#'
#' @param x `list` with project data.
#'
#' @return Invisible `TRUE`.
#'
#' @export
import_data <- function(x) {

  # store variables
  app_data$project <- x$project
  app_data$bbox <- x$project$get_bbox(expand = TRUE)

  # update new solution sidebar
  output$newSolutionPane_settings <- renderSolutionSettings(
    solutionSettings(app_data$project)
  )

  # add modal to edit project data
  shiny::insertUI(
    selector = "#dataModal_pane",
    where = "beforeBegin",
    ui = dataModalPane(
      action_ids = app_data$project$action_ids,
      action_descriptions = app_data$project$action_descriptions,
      parameters = app_data$parameters
    )
  )

  # add modal to show solution data and results
  shiny::insertUI(
    selector = "#solutionModal_pane",
    where = "beforeBegin",
    ui = solutionModalPane(
      action_ids = app_data$project$action_ids,
      action_descriptions = app_data$project$action_descriptions,
      parameters = app_data$parameters
    )
  )

  # update map
  map <- leaflet::leafletProxy("map")
  leaflet::flyToBounds(
    map, app_data$bbox$xmin, app_data$bbox$ymin,
    app_data$bbox$xmax, app_data$bbox$ymax
  )
  leaflet::fitBounds(
    map, app_data$bbox$xmin, app_data$bbox$ymin,
    app_data$bbox$xmax, app_data$bbox$ymax
  )
  app_data$project$render_on_map(map, data = "status")

  # update tables with project data
  output$data_modal_site_table <- rhandsontable::renderRHandsontable({
    app_data$project$render_site_data()
  })
  output$data_modal_feature_table <- rhandsontable::renderRHandsontable({
    app_data$project$render_feature_data()
  })
  output$data_modal_feasibility_table <- rhandsontable::renderRHandsontable({
    app_data$project$render_feasibility_data()
  })
  lapply(seq(app_data$project$get_action_ids()), function(i) {
    output[[paste0("data_action_", i, "_table")]] <-
      rhandsontable::renderRHandsontable({
        app_data$project$render_action_expectation_data(
          action_id = app_data$project$get_action_ids()[[i]]
        )
      })
  })

  # add listeners for project data
  shiny::observeEvent(input$data_modal_site_table, {
    shiny::req(input$data_modal_site_table)
    app_data$project$set_site_data(
      rhandsontable::hot_to_r(input$data_modal_site_table)
    )
  })
  shiny::observeEvent(input$data_modal_feature_table, {
    shiny::req(input$data_modal_feature_table)
    app_data$project$set_feature_data(
      rhandsontable::hot_to_r(input$data_modal_feature_table)
    )
  })
  shiny::observeEvent(input$data_modal_feasibility_table, {
    shiny::req(input$data_modal_feasibility_table)
    app_data$project$set_feasibility_data(
      rhandsontable::hot_to_r(input$data_modal_feasibility_table)
    )
  })
  for (i in seq_along(app_data$project$get_action_ids())) {
    shiny::observeEvent(input[[paste0("data_action_", i, "_table")]], {
      x <- input[[paste0("data_action_", i, "_table")]]
      shiny::req(x)
      app_data$project$set_action_expectation_data(
        rhandsontable::hot_to_r(x),
        action_id = app_data$project$get_action_ids()[[i]]
      )
    })
  }

  # make sidebars visible
  shinyjs::runjs("$('#mainSidebar').css('display','block');")

  # open sidebars
  leaflet.extras2::openSidebar(
    map = map,
    id = "newSolutionPane",
    sidebar_id = "mainSidebar"
  )

  # remove startup mode
  ## this makes the buttons and scalebar visible
  shinyjs::runjs("document.body.classList.remove('startup');")

  # return success
  invisible(TRUE)
}
