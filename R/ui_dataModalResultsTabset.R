#' @include internal.R
NULL

#' Data modal results tabset
#'
#' Constructs a tabset for viewing the results of a solution.
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
dataModalResultsTab <- function(action_ids,
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
    ## parameters
    is.list(parameters)
  )

  # create results tab panels
  panels <- list(
    shiny::tabPanel(
      shiny::span(
        parameters$summary_results_sheet$sheet_name,
        id = "data_modal_results_summary_panel"
      ),
      DT::DTOutput(
        "data_modal_results_summary_table"
      )
    ),
    shiny::tabPanel(
      shiny::span(
        parameters$site_results_sheet$sheet_name,
        id = "data_modal_results_site_panel"
      ),
      DT::DTOutput(
        "data_modal_results_site_table"
      )
    ),
    shiny::tabPanel(
      shiny::span(
        parameters$feature_results_sheet$sheet_name,
        id = "data_modal_results_feature_panel"
      ),
      DT::DTOutput(
        "data_modal_results_feature_table"
      )
    )
  )

  # build tabset
  tabset <- do.call(
    shiny::tabsetPanel,
    append(
      list(id = "data_modal_results_tabset_panels", type = "pills"),
      panels
    )
  )

  # make tooltips
  tooltips <- list(
    shinyBS::bsTooltip(
      "data_modal_results_summary_panel",
      "Table containing summary results for the solution"
    ),
    shinyBS::bsTooltip(
      "data_modal_results_site_table",
      "Table containing site results for the solution"
    ),
    shinyBS::bsTooltip(
      "data_modal_results_feature_table",
      "Table containing feature results for the solution"
    )
  )

  # return result
  shiny::tagList(tabset, tooltips)
}
