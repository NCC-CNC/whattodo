#' @include internal.R
NULL

#' Export sidebar pane
#'
#' Constructs a sidebar pane for exporting data.
#'
#' @inheritParams solutionResultsSidebarPane
#'
#' @inherit importModal examples
#'
#' @inherit solutionResultsSidebarPane details return
#'
#' @export
exportSidebarPane <- function(id) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(id),
    assertthat::noNA(id)
  )

  # create sidebar
  leaflet.extras2::sidebar_pane(
    title = "Download data",
    id = id,
    icon = NULL,

    # sidebar pane content
    htmltools::tags$div(
      class = "sidebar-pane-content",
      htmltools::tags$script(paste0("
        $('a[href=\"#", id, "\"]').tooltip({
          container: 'body',
          trigger: 'hover',
          placement: 'right',
          title: 'Open sidebar for downloading data and solutions'
        });
      ")),
      htmltools::tags$div(
        class = "sidebar-pane-inner",
        htmltools::tags$div(
          class = "generic-container",
          ## select data
          shiny::selectInput(
            inputId = paste0(id, "_select"),
            label = "Select download option",
            choices = "NA",
            multiple = FALSE,
            width = "100%"
          ),
          ## download button
          htmltools::tags$div(
            class = "col text-center",
            shiny::downloadButton(
              outputId = paste0(id, "_button"),
              label = "Download",
              class = "btn-primary btn-block",
              icon = shiny::icon("download")
            )
          )
        )
      )
    )
  )
}
