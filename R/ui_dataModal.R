#' @include internal.R
NULL

#' Data modal
#'
#' Constructs a modal for viewing and updating data.
#'
#' @param id `character` identifier for the modal.
#'
#' @param trigger `character` identifier for the widget to trigger the modal.
#"
#' @param action_ids `character` identifiers for actions.
#'
#' @param action_descriptions `character` descriptions of actions.
#'
#' @param parameters `list` object with parameters for customizing appearance.
#'
#' @examples
#' \dontrun{
#' # run Shiny app to demo the sidebar pane
#' if (interactive()) {
#'   runExample("dataModal")
#' }
#' }
#'
#' @export
dataModal <- function(id,
                      trigger,
                      action_ids,
                      action_descriptions,
                      parameters) {
  # assert arguments are valid
  assertthat::assert_that(
    ## modal id
    assertthat::is.string(id),
    assertthat::noNA(id),
    ## modal id
    assertthat::is.string(trigger),
    assertthat::noNA(trigger),
    ## action_ids
    is.character(action_ids),
    assertthat::noNA(action_ids),
    length(action_ids) > 0,
    identical(anyDuplicated(action_ids), 0L),
    ## action_descriptions
    is.character(action_descriptions),
    assertthat::noNA(action_descriptions),
    length(action_descriptions) > 0,
    identical(anyDuplicated(action_descriptions), 0L),
    ## parameters
    is.list(parameters)
  )

  # create tab panels
  ## panels that remain fixed across all datasets
  panels <- list(
    shiny::tabPanel(
      shiny::span(
        parameters$site_data_sheet$sheet_name,
        id = "data_modal_site_panel"
      ),
      rhandsontable::rHandsontableOutput("data_modal_site_table")
    ),
    shiny::tabPanel(
      shiny::span(
        parameters$feasibility_data_sheet$sheet_name,
        id = "data_modal_feasibility_panel"
      ),
      rhandsontable::rHandsontableOutput("data_modal_feasibility_table")
    ),
    shiny::tabPanel(
      shiny::span(
        parameters$feature_data_sheet$sheet_name,
        id = "data_modal_feature_panel"
      ),
      rhandsontable::rHandsontableOutput("data_modal_feature_table")
    )
  )

  ## additional panels that depend on the datasets
  panels <- append(panels, lapply(seq_along(action_ids), function(i) {
    ### determine name of sheet
    n <- as.character(glue::glue(
      parameters$action_expectation_sheet$sheet_name,
      action_ids = action_ids[[i]]
    ))
    ## return panel
    shiny::tabPanel(
      shiny::span(n, id = paste0("action_", i, "_panel")),
      rhandsontable::rHandsontableOutput(paste0("action_", i, "_widget"))
    )
  }))

  # create panels
  panels <- do.call(
    shiny::tabsetPanel, append(list(id = "data_model_tabset"), panels)
  )

  # return result
  htmltools::tags$div(
    # script to style modal backdrop
    htmltools::tags$script(htmltools::HTML(paste0(
      "$('#", id, "').on('show.bs.modal', function() {",
      "  setTimeout(function() {",
      "  $('#", id, "').appendTo('body');",
      "$('.modal-backdrop').addClass('sbs-modal-backdrop');",
      "})});"
    ))),
    class = "data-modal modal sbs-modal fade",
    id = id,
    tabindex = "-1",
    `data-sbs-trigger` = trigger,
    # modal content
    do.call(
      shiny::tagList,
      append(
        list(
          panels,
          shinyBS::bsTooltip(
            "data_modal_site_panel",
            "Table containing data for each site"
          ),
          shinyBS::bsTooltip(
            "data_modal_feasibility_panel",
            paste(
              "Table indicating which actions can potentially be",
              "implemented within each site"
            )
          ),
          shinyBS::bsTooltip(
            "data_modal_feature_panel",
            "Table containing data for each feature"
          )
        ),
        lapply(seq_along(action_ids), function(i) {
          shinyBS::bsTooltip(
            paste0("action_", i, "_panel"),
            paste0(
              "Table containing the expected amount of each feature within ",
              " each site if the \"",
              action_ids[[i]],
              "\" action were implemented within the site"
            )
          )
        })
      )
    )
  )
}
