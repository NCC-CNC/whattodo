#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # initialize application
  eval(server_initialize_app)

  # data import
  eval(server_import_manual_data)
  eval(server_import_builtin_data)

  # solution settings
  eval(server_update_solution_settings)

  # solution results
  eval(server_update_solution_results)

  # prioritizations
  eval(server_generate_new_solution)

  # map behavior
  eval(server_update_map)

  # data export
  eval(server_export_data)

}
