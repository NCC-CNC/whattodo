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

  # define application data
  ## note that we use an environment here because they are mutable objects and
  ## so we don't have to worry about using the super-assignment operator
  app_data <- list2env(
    list(
      project = NULL,
      project_data_id = uuid::UUIDgenerate(),
      solution = list(),
      solution_ids = c(),
      parameters = whatdataio::read_data_configuration(),
      bd = c(0, 0)
    )
  )

  # activate start up mode
  ## hides leaflet buttons + scalebar
  shinyjs::runjs("document.body.classList.add('startup');")

  # make sidebars hidden
  shinyjs::runjs("$('#dataSidebar').css('display','none');")
  shinyjs::runjs("$('#analysisSidebar').css('display','none');")

  # display import modal on start up
  shiny::showModal(importModal(id = "importModal"))

  # initialize map
  output$map <- leaflet::renderLeaflet(
    leaflet_map(c("dataSidebar", "analysisSidebar"))
  )

  # initialize widgets
  output$solutionResultsPane_results <- renderSolutionResults({
    solutionResults()
  })

  # initialize listener
  map_listener <- shiny::reactiveVal(runif(1))

  # initialize built in projects
  if (nrow(project_data) > 0) {
    ## update select input with project names
    shiny::updateSelectInput(
      inputId = "importModal_name",
      choices = stats::setNames(project_data$path, project_data$name)
    )
  } else {
    ## disable import button since no available projects
    disable_html_element("importModal_builtin_button")
  }

  # initialize export options
  shiny::updateSelectizeInput(
    session = session,
    inputId = "exportPane_select",
    selected = app_data$project_data_id,
    choices = c(
      stats::setNames(app_data$project_data_id, "Project data")
    )
  )

  # disable buttons that require inputs
  disable_html_element("importModal_manual_button")
  shinyjs::disable("newSolutionPane_settings_stop_button")

  # disable solution results sidebar button
  disable_html_css_selector("#analysisSidebar li:nth-child(2)")

  # add help modal button trigger
  observeEvent(input$help_button, {
    shinyBS::toggleModal(
      session = session, modalId = "helpModal", toggle = "open"
    )
  })

  # add data modal trigger for new solution data
  observeEvent(input$newSolutionPane_settings_edit_button, {
    ## specify dependencies
    shiny::req(input$newSolutionPane_settings_edit_button)

    ## update select input so that tables for new solution data are shown
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "dataModal_select",
      selected = app_data$project_data_id
    )

    ## open modal
    shinyBS::toggleModal(
      session = session, modalId = "dataModal", toggle = "open"
    )
  })

  # add data modal trigger for existing solution data
  observeEvent(input$solutionResultsPane_results_button, {
    ## specify dependencies
    shiny::req(input$solutionResultsPane_results_button)

    ## update select input so that tables for new solution data are shown
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "dataModal_select",
      selected = input$solutionResultsPane_results_select
    )

    ## open modal
    shinyBS::toggleModal(
      session = session, modalId = "dataModal", toggle = "open"
    )
  })

})
