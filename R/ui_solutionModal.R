#' @include internal.R
NULL

#' Solution modal
#'
#' Constructs a modal for viewing solution data and results.
#'
#' @param id `character` identifier for the modal.
#'
#' @param trigger `character` identifier for the widget to trigger the modal.
#'
#' @examples
#' \dontrun{
#' # run Shiny app to demo the sidebar pane
#' if (interactive()) {
#'   runExample("solutionModal")
#' }
#' }
#'
#' @export
solutionModal <- function(id,
                          trigger) {
  # assert arguments are valid
  assertthat::assert_that(
    ## modal id
    assertthat::is.string(id),
    assertthat::noNA(id),
    ## modal id
    assertthat::is.string(trigger),
    assertthat::noNA(trigger)
  )

  # create modal
  out <- htmltools::tags$div(
    # script to style modal backdrop
    htmltools::tags$script(htmltools::HTML(paste0(
      "$('#", id, "').on('show.bs.modal', function() {",
      "  setTimeout(function() {",
      "  $('#", id, "').appendTo('body');",
      "$('.modal-backdrop').addClass('sbs-modal-backdrop');",
      "})});"
    ))),
    class = "solution-modal modal sbs-modal fade",
    id = id,
    tabindex = "-1",
    `data-sbs-trigger` = trigger,
    # modal content
    htmltools::tags$div(
      class = "modal-dialog modal-xl",
      htmltools::tags$div(
        class = "modal-content",
        ## header
        htmltools::tags$div(
          class = "modal-header",
          ### close button
          htmltools::tags$button(
            type = "button",
            class = "close",
            `data-dismiss` = "modal",
            htmltools::tags$span(htmltools::HTML("&times;"))
          ),
          htmltools::tags$div(
            class = "modal-header-content",
            htmltools::tags$div(
              class = "select-container",
              horizontalPickerInput(
                inputId = paste0(id, "_select"),
                choices = "NA",
                label = "Solution:",
                multiple = FALSE,
                options = list(`container` = "body")
              )
            )
          )
        ),
        ## body
        htmltools::tags$div(
          class = "modal-body",
          htmltools::div(id = paste0(id, "_pane"))
        )
      )
    )
  )

  # attach dependencies
  htmltools::attachDependencies(
    out,
    htmltools::htmlDependencies(shinyBS::bsModal("x", "y", "z"))
  )

}
