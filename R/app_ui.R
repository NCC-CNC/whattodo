#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shiny::fluidPage(
      shinyBS::bsTooltip(
        "data_tab",
        paste("Input data used to generate a prioritization")),
      shinyBS::bsTooltip(
        "results_tab",
        paste("Results associated with a prioritization")),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::h3("What To Do"),
          shiny::br(),
          shiny::uiOutput("sidebar_ui")
        ),
        shiny::mainPanel(
          shiny::uiOutput("map_ui"),
          shinyBS::bsModal(
            id = "alert_modal",
            trigger = "alert_modal_trigger",
            size = "large",
            title = shiny::h4(shiny::textOutput("alert_modal_title")),
            shiny::p(shiny::textOutput("alert_modal_msg"))
          ),
          shiny::tabsetPanel(
            id = "main_tabset",
            type = "tabs",
            shiny::tabPanel(
              shiny::span(
                id = "data_tab",
                "Data"
              ),
              shiny::uiOutput("data_ui")
            ),
            shiny::tabPanel(
              shiny::span(
                id = "results_tab",
                "Results"
              ),
              shiny::uiOutput(
                "results_ui"
              )
            )
          )
        )
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

    ## favicon
    golem::favicon(),

    ## dependencies
    shinyjs::useShinyjs()
  )
}
