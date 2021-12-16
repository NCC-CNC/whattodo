#' @include internal.R
NULL

#' Solution modal pane
#'
#' Constructs a pane for the solution modal.
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
solutionModalPane <- function(action_ids,
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

  # create data tab panels
  ## panels that remain fixed across all datasets
  data_panels <- list(
    shiny::tabPanel(
      shiny::span(
        parameters$site_data_sheet$sheet_name,
        id = "solution_data_modal_site_panel"
      ),
      rhandsontable::rHandsontableOutput(
        "solution_data_modal_site_table"
      )
    ),
    shiny::tabPanel(
      shiny::span(
        parameters$feasibility_data_sheet$sheet_name,
        id = "solution_data_modal_feasibility_panel"
      ),
      rhandsontable::rHandsontableOutput(
        "solution_data_modal_feasibility_table"
      )
    ),
    shiny::tabPanel(
      shiny::span(
        parameters$feature_data_sheet$sheet_name,
        id = "solution_data_modal_feature_panel"
      ),
      rhandsontable::rHandsontableOutput(
        "solution_data_modal_feature_table"
      )
    )
  )

  ## additional panels that depend on the datasets
  data_panels <- append(data_panels, lapply(seq_along(action_ids), function(i) {
    ### determine name of sheet
    n <- as.character(glue::glue(
      parameters$action_expectation_sheet$sheet_name,
      action_ids = action_ids[[i]]
    ))
    ## return panel
    shiny::tabPanel(
      shiny::span(n, id = paste0("solution_data_action_", i, "_panel")),
      rhandsontable::rHandsontableOutput(
        paste0("solution_data_action_", i, "_table")
      )
    )
  }))

  # create solution tab panels
  ## create panels
  results_panels <- list(
    shiny::tabPanel(
      shiny::span(
        parameters$summary_results_sheet$sheet_name,
        id = "solution_results_modal_summary_panel"
      ),
      DT::DTOutput(
        "solution_results_modal_summary_table"
      )
    ),
    shiny::tabPanel(
      shiny::span(
        parameters$site_results_sheet$sheet_name,
        id = "solution_results_modal_site_panel"
      ),
      DT::DTOutput(
        "solution_results_modal_site_table"
      )
    ),
    shiny::tabPanel(
      shiny::span(
        parameters$feature_results_sheet$sheet_name,
        id = "solution_results_modal_feature_panel"
      ),
      DT::DTOutput(
        "solution_results_modal_feature_table"
      )
    )
  )


  ## data tabset
  data_tabset <- do.call(
    shiny::tabsetPanel,
    append(
      list(id = "solution_data_tabset"),
      data_panels
    )
  )

  ## results tabset
  results_tabset <- do.call(
    shiny::tabsetPanel,
    append(
      list(id = "solution_results_tabset"),
      results_panels
    )
  )

  ## data tooltips
  tooltips <- append(
    list(
      shinyBS::bsTooltip(
        "solution_data_modal_site_panel",
        "Table containing site data used to generate solution"
      ),
      shinyBS::bsTooltip(
        "solution_data_modal_feasibility_panel",
        "Table containing feasibility data used to generate solution"
      ),
      shinyBS::bsTooltip(
        "solution_data_modal_feature_panel",
        "Table containing feature data used to generate solution"
      ),
      shinyBS::bsTooltip(
        "solution_results_modal_summary_panel",
        "Table containing summary results for the solution"
      ),
      shinyBS::bsTooltip(
        "solution_results_modal_site_table",
        "Table containing site results for the solution"
      ),
      shinyBS::bsTooltip(
        "solution_results_modal_feature_table",
        "Table containing feature results for the solution"
      )
    ),
    lapply(seq_along(action_ids), function(i) {
      shinyBS::bsTooltip(
        paste0("solution_data_action_", i, "_table"),
        paste0(
          "Table containing the expectation data for the",
          "\"", action_ids[[i]],
          "\" action that was used to generate solution"
        )
      )
    })
  )

  # return result
  shiny::tagList(
    shiny::tabsetPanel(
      id = "solution_tabset",
      shiny::tabPanel(
        "Data used to generate solution",
        data_tabset
      ),
      shiny::tabPanel(
        "Statistics describing solution",
        results_tabset
      )
    ),
    tooltips
  )
}
