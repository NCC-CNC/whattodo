#' @include internal.R
NULL

#' Data modal project tabset
#'
#' Constructs a tabset for updating and viewing the project data used to
#' generate new solutions, and also viewing the data used to generate a
#' new solution.
#'
#' @param action_ids `character` identifiers for actions.
#'
#' @param action_descriptions `character` descriptions of actions.
#'
#' @param parameters `list` object with parameters for customizing appearance.
#'
#' @examples
#' #TODO
#' @export
dataModalProjectTab <- function(action_ids,
                                action_descriptions,
                                parameters) {
  # assert arguments are valid
  assertthat::assert_that(
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
  panels <- list(
    shiny::tabPanel(
      shiny::span(
        parameters$site_data_sheet$sheet_name,
        id = "data_modal_project_site_panel"
      ),
      htmltools::tags$div(
        class = "table-container",
        rhandsontable::rHandsontableOutput(
          "data_modal_project_site_table"
        )
      )
    ),
    shiny::tabPanel(
      shiny::span(
        parameters$feasibility_data_sheet$sheet_name,
        id = "data_modal_project_feasibility_panel"
      ),
      htmltools::tags$div(
        class = "table-container",
        rhandsontable::rHandsontableOutput(
          "data_modal_project_feasibility_table"
        )
      )
    ),
    shiny::tabPanel(
      shiny::span(
        parameters$feature_data_sheet$sheet_name,
        id = "data_modal_project_feature_panel"
      ),
      htmltools::tags$div(
        class = "table-container",
        rhandsontable::rHandsontableOutput(
          "data_modal_project_feature_table"
        )
      )
    )
  )

  # additional tab panels that depend on the datasets
  panels <- append(panels, lapply(seq_along(action_ids), function(i) {
    ### determine name of sheet
    n <- as.character(glue::glue(
      parameters$consequence_sheet$sheet_name,
      action_ids = action_ids[[i]]
    ))
    ## return panel
    shiny::tabPanel(
      shiny::span(n, id = paste0("data_modal_project_action_", i, "_panel")),
      htmltools::tags$div(
        class = "table-container",
        rhandsontable::rHandsontableOutput(
          paste0("data_modal_project_action_", i, "_table")
        )
      )
    )
  }))

  # build tabset
  tabset <- do.call(
    shiny::tabsetPanel,
    append(
      list(id = "data_modal_project_tabset_panels", type = "pills"),
      panels
    )
  )

  # return result
  shiny::tagList(tabset)
}
