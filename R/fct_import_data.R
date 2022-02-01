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
  app_data$bd <- input$browser_dimension

  # update new solution sidebar
  output$newSolutionPane_settings <- renderSolutionSettings(
    solutionSettings(app_data$project)
  )

  # set select input for data modal
  shinyWidgets::updatePickerInput(
    session = session,
    inputId = "dataModal_select",
    selected = app_data$project_data_id,
    choices = stats::setNames(app_data$project_data_id, "Data for new solution")
  )

  # add project data tabset to data modal
  shiny::insertUI(
    selector = "#dataModal_project_tab",
    where = "beforeBegin",
    ui = dataModalProjectTab(
      action_ids = app_data$project$action_ids,
      action_descriptions = app_data$project$action_descriptions,
      parameters = app_data$parameters
    )
  )

  # add results modal tabset to data modal
  shiny::insertUI(
    selector = "#dataModal_results_tab",
    where = "beforeBegin",
    ui = dataModalResultsTab(
      action_ids = app_data$project$action_ids,
      action_descriptions = app_data$project$action_descriptions,
      parameters = app_data$parameters
    )
  )

  # add listener to update tables based on selected dataset
  shiny::observeEvent(input$dataModal_select, ignoreInit = TRUE, {
    ## specify dependencies
    shiny::req(input$dataModal_select)

    ## prepare for rendering data
    if (identical(input$dataModal_select, app_data$project_data_id)) {
      ### set dataset
      d <- app_data$project
      ### update tab titles
      shinyjs::runjs(
        paste0(
          "document.querySelector('",
          "#dataModal_project_title').innerHTML = ",
          "'Data for generating new solution';"
        )
      )
      shinyjs::runjs(
        paste0(
          "document.querySelector('",
          "#dataModal_results_title').innerHTML = ",
          "'Not applicable';"
        )
      )
      ### make second tab invisible
      shinyjs::runjs(
        paste0(
          "document.querySelector('",
          "#dataModal_tabset li:nth-child(2)').style.display = 'none';"
        )
      )
      ### ensure first tab selected
      shiny::updateTabsetPanel(
        session = session,
        inputId = "dataModal_tabset",
        selected = "data_tab"
      )
    } else if (input$dataModal_select %in% app_data$solution_ids) {
      ### set dataset
      i <- which(app_data$solution_ids == input$dataModal_select)
      d <- app_data$solution[[i]]
      ### update tab titles
      shinyjs::runjs(
        paste0(
          "document.querySelector('",
          "#dataModal_project_title').innerHTML = ",
          "'Data used to generate solution';"
        )
      )
      shinyjs::runjs(
        paste0(
          "document.querySelector('",
          "#dataModal_results_title').innerHTML = ",
          "'Statistics describing solution';"
        )
      )
      ### make second tab visible
      shinyjs::runjs(
        paste0(
          "document.querySelector('",
          "#dataModal_tabset li:nth-child(2)').style.display = 'block';"
        )
      )
    } else {
      stop("Dataset not found for data modal")
    }

    ## render datasets
    output$data_modal_project_site_table <-
      rhandsontable::renderRHandsontable({
        d$render_site_data(height = app_data$bd[2] - 350)
      })
    output$data_modal_project_feature_table <-
      rhandsontable::renderRHandsontable({
        d$render_feature_data(height = app_data$bd[2] - 350)
      })
    output$data_modal_project_feasibility_table <-
      rhandsontable::renderRHandsontable({
        d$render_feasibility_data(height = app_data$bd[2] - 350)
      })
    lapply(seq_along(app_data$project$get_action_ids()), function(i) {
      output[[paste0("data_modal_project_action_", i, "_table")]] <-
        rhandsontable::renderRHandsontable({
          d$render_consequence_data(
            action_id = d$get_action_ids()[[i]],
            height = app_data$bd[2] - 350
          )
        })
    })
    if (inherits(d, "Solution")) {
      output$data_modal_results_summary_table <- DT::renderDT({
        d$render_summary_results()
      })
      output$data_modal_results_site_table <- DT::renderDT({
        d$render_site_results()
      })
      output$data_modal_results_feature_table <- DT::renderDT({
        d$render_feature_results()
      })
    } else {
      output$data_modal_results_summary_table <- DT::renderDT({
        tibble::tibble(Message = "Not applicable")
      })
      output$data_modal_results_site_table <- DT::renderDT({
        tibble::tibble(Message = "Not applicable")
      })
      output$data_modal_results_site_table <- DT::renderDT({
        tibble::tibble(Message = "Not applicable")
      })
    }
  })

  # add listeners for updating new solution data
  shiny::observeEvent(input$data_modal_project_site_table, {
    shiny::req(input$data_modal_project_site_table)
    if (!identical(input$dataModal_select, app_data$project_data_id)) return()
    app_data$project$set_site_data(
      rhandsontable::hot_to_r(input$data_modal_project_site_table)
    )
    updateSolutionSettings(
      session = session,
      inputId = "newSolutionPane_settings",
      value = list(
        id = app_data$project$settings[[1]]$id,
        setting = "range",
        value = c(
          app_data$project$get_min_budget(),
          app_data$project$get_max_budget()
        ),
        type = "parameter"
      )
    )
  })
  shiny::observeEvent(input$data_modal_project_feature_table, {
    shiny::req(input$data_modal_project_feature_table)
    if (!identical(input$dataModal_select, app_data$project_data_id)) return()
    app_data$project$set_feature_data(
      rhandsontable::hot_to_r(input$data_modal_project_feature_table)
    )
  })
  shiny::observeEvent(input$data_modal_project_feasibility_table, {
    shiny::req(input$data_modal_project_feasibility_table)
    if (!identical(input$dataModal_select, app_data$project_data_id)) return()
    app_data$project$set_feasibility_data(
      rhandsontable::hot_to_r(input$data_modal_project_feasibility_table)
    )
  })

  lapply(seq_along(app_data$project$get_action_ids()), function(i) {
    shiny::observeEvent(
      input[[paste0("data_modal_project_action_", i, "_table")]],  {
      x <- input[[paste0("data_modal_project_action_", i, "_table")]]
      shiny::req(x)
      if (!identical(input$dataModal_select, app_data$project_data_id)) return()
      app_data$project$set_consequence_data(
        rhandsontable::hot_to_r(x),
        action_id = app_data$project$get_action_ids()[[i]]
      )
    })
  })

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

  # update map data control
  shiny::updateSelectInput(
    session = session,
    inputId = "map_dataset",
    choices = stats::setNames(app_data$project_data_id, "Data for new solution")
  )
  shiny::updateSelectInput(
    session = session,
    inputId = "map_layer",
    choices = app_data$project$get_map_layers()
  )

  # define listener for map
  map_data_listener <- shiny::reactive({
    append(
      list(
        input$data_modal_project_site_table,
        input$data_modal_project_feasibility_table
      ),
      lapply(seq_along(app_data$project$get_action_ids()), function(i) {
       input[[paste0("data_modal_project_action_", i, "_table")]]
     })
   )
  })
  shiny::observeEvent(map_data_listener(), {
    shiny::req(map_data_listener())
    map_listener(stats::runif(1))
  })

  # make sidebars visible
  shinyjs::runjs("$('#mainSidebar').css('display','block');")

  # make layer control visible
  shinyjs::runjs("$('#map_control').css('display','block');")

  # open sidebars
  leaflet.extras2::openSidebar(
    map = map,
    id = "newSolutionPane",
    sidebar_id = "mainSidebar"
  )

  # remove startup mode
  ## this makes the buttons and scale bar visible
  shinyjs::runjs("document.body.classList.remove('startup');")

  # return success
  invisible(TRUE)
}
