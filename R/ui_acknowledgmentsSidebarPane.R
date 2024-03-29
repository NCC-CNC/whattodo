#' @include internal.R widget_solutionSettings_ui.R
NULL

#' Acknowledgments sidebar pane
#'
#' Constructs a sidebar pane for displaying acknowledgments.
#'
#' @inheritParams solutionResultsSidebarPane
#'
#' @inherit solutionResultsSidebarPane details return
#'
#' @examples
#' \dontrun{
#' # run Shiny app to demo the sidebar pane
#' if (interactive()) {
#'   runExample("allSidebars")
#' }
#' }
#'
#' @export
acknowledgmentsSidebarPane <- function(id) {
  # assert arguments are valid
  assertthat::assert_that(
    ### id
    assertthat::is.string(id),
    assertthat::noNA(id)
  )

  # create sidebar widget
  ## create sidebar
  leaflet.extras2::sidebar_pane(
    title = "Acknowledgments",
    id = id,
    icon = NULL,
    htmltools::tags$div(
      class = "sidebar-pane-content",
      htmltools::tags$script(paste0("
        $('a[href=\"#", id, "\"]').tooltip({
          container: 'body',
          trigger: 'hover',
          placement: 'right',
          title: 'Open sidebar with acknowledgments'
        });
      ")),
      htmltools::tags$div(
        class = "sidebar-pane-inner",
        htmltools::tags$div(
          class = "generic-container",
          ## NCC acknowledgments
          shiny::includeMarkdown(
            system.file(
              "app", "text", "acknowledge-ncc.md", package = "whattodo"
            )
          ),
          ## main dependencies
          shiny::includeMarkdown(
            system.file(
              "app", "text", "acknowledge-deps.md", package = "whattodo"
            )
          ),
          ## additional dependencies
          acknowledge_packages(
            x = setdiff(
              rownames(utils::installed.packages()),
              c("prioritizr", "shiny", "whattodo")
            ),
            prefix = "It also uses the following R packages: ",
            suffix = "."
          )
        )
      )
    )
  )
}
