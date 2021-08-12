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
    shiny::fluidPage(

      ## suppress dependencies that fail to import correctly
      htmltools::suppressDependencies("shinyBS"),

      ## add tooltips
      shinyBS::bsTooltip(
        id = "data_tab",
        title = "Input data used to generate a prioritization"
      ),
      shinyBS::bsTooltip(
        id = "results_tab",
        title = "Results associated with a prioritization"
      ),

      # sidebar layout
      shiny::sidebarLayout(

        # sidebar content
        shiny::sidebarPanel(
          shiny::h3("What To Do"),
          shiny::br(),
          shiny::uiOutput("sidebar_ui")
        ),

        # main panel content
        shiny::mainPanel(
          ## map
          shiny::uiOutput("map_ui"),

          ## alert modal
          shinyBS::bsModal(
            id = "alert_modal",
            trigger = "alert_modal_trigger",
            size = "large",
            title = shiny::h4(shiny::textOutput("alert_modal_title")),
            shiny::p(shiny::textOutput("alert_modal_msg"))
          ),

          ## tabset panel
          shiny::tabsetPanel(
            id = "main_tabset",
            type = "tabs",

            ## data tab
            shiny::tabPanel(
              shiny::span(id = "data_tab", "Data"),
              shiny::uiOutput("data_ui")
            ),

            ## result tab
            shiny::tabPanel(
              shiny::span(id = "results_tab", "Results"),
              shiny::uiOutput("results_ui")
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
#' @details
#' This function can also add Google Analytics tracking to the web application.
#' To achieve this, you need to specify the Google Analytics Identifier using
#' the `GOOGLE_ANALYTICS_ID` environmental variable.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  # Google Analytics
  ga_id <- Sys.getenv("GOOGLE_ANALYTICS_ID")
  if (nchar(ga_id) > 0) {
    ga_js <- htmltools::tagList(
      htmltools::tags$script(
        async = "",
        src = paste0("https://www.googletagmanager.com/gtag/js?id=", ga_id)
      ),
      htmltools::tags$script(
        htmltools::HTML(paste0("
        window.dataLayer = window.dataLayer || [];
        function gtag() {dataLayer.push(arguments);}
        gtag(\"js\", new Date());
        gtag(\"config\", \"", ga_id, "\");
        "))
      )
    )
  }

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

    ## Google Analytics
    if (nchar(ga_id) > 0) {
      ga_js
    },

    ## favicon
    golem::favicon(),

    ## dependencies
    shinyjs::useShinyjs()
  )
}
