#' @include internal.R
NULL

#' Initial sidebar user interface
#'
#' Create a user interface component for uploading data to the the application.
#'
#' @return A `shiny.tag` object.
#'
#' @export
initial_sidebar_ui <- function() {
  # create ui
  shiny::tagList(
    shiny::helpText("Welcome to the What To Do application! This application is designed to help you prioritize implementation of management actions in conservation areas (termed sites). Please understand that this app requires input data in a very specific format. If you haven't already, please use the Action Data Template application to generate a custom Excel Spreadsheet template for your study area. After populating the Excel Spreadsheet with the required data, please upload it to this app using the buttons below. If you have a shapefile delineating the spatial locations (or boundaries) of each site, you can also upload it too (please note that you will need to supply a \".shp\", \".shx\",  \".dbf\", and \".prj\" file during upload). Once you've finished uploading the data, please click the \"Ready to go!\" button to start generating optimized conservation management plans."),
    shiny::br(),
    shiny::h4("Data upload"),
    shiny::wellPanel(
      shiny::fileInput(
        inputId = "spreadsheet_upload_widget",
        "Choose Excel Spreadsheet",
        multiple = FALSE,
        accept = ".xlsx"
      ),
      shiny::checkboxInput(
        inputId = "shapefile_query_widget",
        "Use a shapefile for site locations?",
        value = FALSE
      ),
      shiny::conditionalPanel(
        condition = "input.shapefile_query_widget == 1",
        shiny::fileInput(
          inputId = "shapefile_upload_widget",
          "Choose shapefile",
          multiple = TRUE,
          accept = c(
            ".shp", ".dbf", ".prj", ".shx", ".cpg", ".sbn", ".sbx",
            ".shp.xml"
          )
        )
      ),
    ),
    shiny::div(
      shinyBS::bsButton(
        "data_done_btn", "Ready to go!", style = "primary", disabled = TRUE
      ),
      class = "divCenter"
    ),
    shiny::br(),
    shiny::div(
      shinyBS::bsTooltip(
        "data_done_btn", "Click this button to upload the data"
      )
    )
  )
}

#' Initial data user interface
#'
#' Create a user interface component as a place-holder to display data before
#' any data are uploaded to the application.
#'
#' @return A `shiny.tag` object.
#'
#' @export
initial_data_ui <- function() {
  shiny::tagList(shiny::wellPanel())
}

#' Initial result user interface
#'
#' Create a user interface component as a place-holder to display results
#' before any data are uploaded to the application.
#'
#' @return A `shiny.tag` object.
#'
#' @export
initial_results_ui <- function() {
  shiny::tagList(shiny::wellPanel())
}

#' Initial map user interface
#'
#' Create a user interface component as a place-holder to display
#' a map before any data are uploaded to the application.
#'
#' @return A `shiny.tag` object.
#'
#' @export
initial_map_ui <- function() {
  shiny::tagList(shiny::wellPanel(shiny::tags$div(class = "divMap")))
}
