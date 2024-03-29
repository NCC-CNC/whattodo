#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    # add external resources
    golem_add_external_resources(),

    # app content
    shiny::fillPage(

      ## enable functionality from shinyFeedback package
      shinyFeedback::useShinyFeedback(),

      ## suppress dependencies that fail to import correctly
      htmltools::suppressDependencies("shinyBS"),
      htmltools::suppressDependencies("bootstrap-select"),

      ## manually insert code dependencies so they import correctly
      htmltools::tags$head(
        ### shinyBS just doesn't work inside Docker containers
        htmltools::tags$script(src = "www/shinyBS-copy.js"),
        ### shinyWidgets has invalid SourceMap configuration
        htmltools::tags$script(src = "www/bootstrap-select-copy.min.js"),
        htmltools::tags$link(
          rel = "stylesheet",
          type = "text/css",
          href = "www/bootstrap-select-copy.min.css"
        )
      ),

      ## title
      shiny::h3(
        "What To Do",
        id = "app_title",
        class = "leaflet-title"
      ),

      ## map user interface
      htmltools::tags$div(
        id = "map_control",
        class = "leaflet-data-control",
        shiny::selectInput(
          inputId = "map_dataset",
          label = "Select dataset",
          choices = "NA"
        ),
        shiny::selectInput(
          inputId = "map_layer",
          label = "Select layer",
          choices = "NA"
        )
      ),

      ## start up screen
      shinybusy::busy_start_up(
        loader = shinybusy::spin_epic("scaling-squares", color = "#FFF"),
        text = "Loading...",
        mode = "auto",
        color = "#FFF",
        background = "#001329"
      ),

      ## leaflet map
      leaflet::leafletOutput("map", width = "100%", height = "100%"),

      ## help modal
      helpModal("helpModal", trigger = "help_button"),

      ## data modal
      dataModal("dataModal", trigger = "data_button"),

      ## error modal
      errorModal("errorModal", trigger = "error_button"),

      ## sidebar
      leaflet.extras2::sidebar_tabs(
        id = "mainSidebar",
        iconList = list(
          shiny::icon("rocket"),
          shiny::icon("tachometer-alt"),
          shiny::icon("download"),
          shiny::icon("envelope"),
          shiny::icon("heart")
        ),
        newSolutionSidebarPane(id = "newSolutionPane"),
        solutionResultsSidebarPane(id = "solutionResultsPane"),
        exportSidebarPane(id = "exportPane"),
        contactSidebarPane(id = "contactPane"),
        acknowledgmentsSidebarPane(id = "acknowledgmentsPane")
      )
    )

  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  # add resources
  add_resource_path(
    "www", app_sys("app/www")
  )

  # define HTML tags in header
  tags$head(
    ## bundle CSS and JS files
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "What To Do"
    ),

    ## dependencies
    shinyjs::useShinyjs(),
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert()

  )
}
