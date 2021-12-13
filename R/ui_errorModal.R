#' @include internal.R widget_solutionResults_ui.R
NULL

#' Solution error modal
#'
#' Constructs a modal for displaying error information.
#'
#' @param id `character` identifier for the modal.
#'
#' @param trigger `character` identifier for the modal trigger.
#'   See [shinyBS::bsModal] for further details.
#'
#' @return A `shiny.tag` object with the modal
#'
#' @export
errorModal <- function(id, trigger) {
  # create modal
  out <-
    htmltools::tags$div(
      # script to style modal backdrop
      htmltools::tags$script(htmltools::HTML(paste0(
        "$('#", id, "').on('show.bs.modal', function() {",
        "  setTimeout(function() {",
        "  $('#", id, "').appendTo('body');",
        "$('.modal-backdrop').addClass('sbs-modal-backdrop');",
        "})});"
      ))),
      class = "error-modal modal sbs-modal fade",
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
              htmltools::h4("No solution exists")
            )
          ),
          ## body
          htmltools::tags$div(
            class = "modal-body",
            suppressWarnings(
              shiny::includeMarkdown(
                system.file(
                  "app", "text", "help.md", package = "whatodo"
                )
              )
            )
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
