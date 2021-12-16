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
      parameters = whatdataio::read_data_configuration()
    )
  )

  # activate start up mode
  ## hides leaflet buttons + scalebar
  shinyjs::runjs("document.body.classList.add('startup');")

  # make sidebars hidden
  shinyjs::runjs("$('#mainSidebar').css('display','none');")

  # display import modal on start up
  shiny::showModal(importModal(id = "importModal"))

  # initialize map
  output$map <- leaflet::renderLeaflet(leaflet_map("mainSidebar"))

  # initialize widgets
  output$solutionResultsPane_results <- renderSolutionResults({
    solutionResults()
  })

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

  # disable buttons that require inputs
  disable_html_element("importModal_manual_button")
  shinyjs::disable("newSolutionPane_settings_stop_button")

  # disable solution results sidebar button
  disable_html_css_selector("#mainSidebar li:nth-child(2)")

  # add help modal button trigger
  observeEvent(input$help_button, {
    shinyBS::toggleModal(
      session = session, modalId = "helpModal", toggle = "open"
    )
  })

})
