#' @include internal.R
NULL

#' Import modal
#'
#' Constructs a modal for importing data.
#'
#' @param id `character` identifier.
#'
#' @return A `shiny.tag` object.
#'
#' @examples
#' \dontrun{
#' # run Shiny app to demo the sidebar pane
#' if (interactive()) {
#'   runExample("importModal")
#' }
#' }
#'
#' @export
importModal <- function(id) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(id),
    assertthat::noNA(id)
  )

  # create modal
  shiny::modalDialog(
    title = htmltools::tags$p(
      "Welcome to What To Do",
      style = "text-align:center"
    ),
    easyClose = FALSE,
    fade = TRUE,
    footer = htmltools::tags$div(
      # styling
      style = "text-align: center",
      # builtin button
      shiny::conditionalPanel(
        condition = paste0("input.", id, "_method == 'builtin'"),
        shinyFeedback::loadingButton(
          inputId = paste0(id, "_builtin_button"),
          label = "Import",
          loadingLabel = "Loading..."
        )
      ),
      # manual button
      shiny::conditionalPanel(
        condition = paste0("input.", id, "_method == 'manual'"),
        shinyFeedback::loadingButton(
          inputId = paste0(id, "_manual_button"),
          label = "Import",
          loadingLabel = "Loading..."
        ),
      )

    ),

    ## import method
    shiny::selectInput(
      inputId = paste0(id, "_method"),
      label = "Select import method",
      choices = c(
        "built-in project" = "builtin",
        "upload project data" = "manual"
      ),
      selected = "built-in project",
      multiple = FALSE
    ),

    ## builtin method
    shiny::conditionalPanel(
      ### condition
      condition = paste0("input.", id, "_method == 'builtin'"),
      ### main
      shiny::selectInput(
        inputId = paste0(id, "_name"),
        label = "Select project",
        choices = c("No built-in projects available" = "NA"),
        multiple = FALSE
      )
    ),

    ## spreadsheet method
    shiny::conditionalPanel(
      ### condition
      condition = paste0("input.", id, "_method == 'manual'"),
      ### main
      shiny::fileInput(
        paste0(id, "_manual_spatial_file"),
        "Select spatial data",
        multiple = TRUE,
        accept = c(".shp", ".shx", ".prj", ".dbf", ".cpg", ".zip")
      ),
      shiny::fileInput(
        paste0(id, "_manual_spreadsheet_file"),
        "Select spreadsheet data",
        multiple = FALSE,
        accept = c(".xlsx")
      )
    )

  )
}
