#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # initialize application
  eval(server_initialize_app)

  # file upload
  eval(server_upload_shapefile)
  eval(server_upload_spreadsheet)

  # data import
  eval(server_import_data)

  # prioritizations
  eval(server_generate_prioritization)

  # map behavior
  eval(server_update_map)

  # data export
  eval(server_export_data)

}
