#' @include internal.R
NULL

#' Results user interface
#'
#' Create a user interface for viewing the results associated with a
#' prioritizations.
#'
#' @param site_names `character` names of sites.
#'
#' @param feature_names `character` names of features.
#'
#' @param action_names `character` names of actions.
#'
#' @param parameters `list` object with parameters for customizing appearance.
#'
#' @return A `shiny.tag` object.
#'
#' @export
primary_results_ui <- function(
  site_names, feature_names, action_names, parameters) {
  # assert arguments are valid
  assertthat::assert_that(
    ## site_names
    is.character(site_names),
    assertthat::noNA(site_names),
    length(site_names) > 0,
    identical(anyDuplicated(site_names), 0L),
    ## feature_names
    is.character(feature_names),
    assertthat::noNA(feature_names),
    length(feature_names) > 0,
    identical(anyDuplicated(feature_names), 0L),
    ## action_names
    is.character(action_names),
    assertthat::noNA(action_names),
    length(action_names) > 0,
    identical(anyDuplicated(action_names), 0L),
    ## parameters
    is.list(parameters)
  )

  # create tab panels
  ## panels that remain fixed across all datasets
  panels <- list(
    shiny::tabPanel(
      shiny::span(
        parameters$summary_results_sheet$sheet_name,
        id = "summary_results_panel"
      ),
      rhandsontable::rHandsontableOutput("summary_results_widget")
    ),
    shiny::tabPanel(
      shiny::span(
        parameters$site_results_sheet$sheet_name,
        id = "site_results_panel"
      ),
      rhandsontable::rHandsontableOutput("site_results_widget")
    ),
    shiny::tabPanel(
      shiny::span(
        parameters$feature_results_sheet$sheet_name,
        id = "feature_results_panel"
      ),
      rhandsontable::rHandsontableOutput("feature_results_widget")
    )
  )

  # create panels
  panels <- do.call(
    shiny::tabsetPanel, append(list(id = "results_tabset_ui"), panels)
  )

  # return result
  shiny::tagList(
    shinyBS::bsTooltip(
      "summary_results_panel",
      "Table summarizing the prioritization"
    ),
    shinyBS::bsTooltip(
      "site_results_panel",
      "Table indicating priority actions for each site"
    ),
    shinyBS::bsTooltip(
      "feature_results_panel",
      paste(
        "Table describing the total amount expected for each feature",
        "if the prioritization was implemented"
      )
    ),
    panels
  )
}
