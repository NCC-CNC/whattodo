#' @include internal.R widget_solutionSettings_ui.R
NULL

#' Map sidebar pane
#'
#' Constructs a sidebar pane for showing data on the map.
#'
#' @inheritParams solutionResultsSidebarPane
#'
#' @examples
#' # TODO
#'
#' @export
mapSidebarPane <- function(id,
                           dataset_id = paste0(id, "_dataset"),
                           layer_id = paste0(id, "_layer")) {
  # assert arguments are valid
  assertthat::assert_that(
    ## id
    assertthat::is.string(id),
    assertthat::noNA(id),
    ## dataset_id
    assertthat::is.string(dataset_id),
    assertthat::noNA(dataset_id),
    ## layer_id
    assertthat::is.string(layer_id),
    assertthat::noNA(layer_id)
  )

  # create sidebar
  w <-
    leaflet.extras2::sidebar_pane(
      title = "Table of Contents",
      id = id,
      icon = NULL,
      ### container
      htmltools::tags$div(
        class = "sidebar-pane-content",
        htmltools::tags$script(paste0("
          $('a[href=\"#", id, "\"]').tooltip({
            container: 'body',
            trigger: 'hover',
            placement: 'right',
            title: 'Open sidebar to visualize data and solutions'
          });
        ")),
      htmltools::tags$div(
        class = "sidebar-pane-inner",
        htmltools::tags$div(
          class = "generic-container",
          htmltools::tags$div(
            shiny::selectInput(
              inputId = dataset_id,
              label = "Select dataset",
              choices = "NA"
            ),
            shiny::selectInput(
              inputId = layer_id,
              label = "Select layer",
              choices = "NA"
            )
          )
        )
      )
    )
  )

  # return result
  w
}
