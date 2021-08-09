#' @include internal.R
NULL

#' Data user interface
#'
#' Create a user interface for viewing and editing the input data for
#' developing prioritizations.
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
primary_data_ui <- function(
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
        parameters$site_data_sheet$sheet_name,
        id = "site_data_panel"
      ),
      rhandsontable::rHandsontableOutput("site_data_widget")
    ),
    shiny::tabPanel(
      shiny::span(
        parameters$site_status_sheet$sheet_name,
        id = "site_status_panel"
      ),
      rhandsontable::rHandsontableOutput("site_status_widget")
    ),
    shiny::tabPanel(
      shiny::span(
        parameters$feature_data_sheet$sheet_name,
        id = "feature_data_panel"
      ),
      rhandsontable::rHandsontableOutput("feature_data_widget")
    )
  )

  ## additional panels that depend on the datasets
  panels <- append(panels, lapply(seq_along(action_names), function(i) {
    ### determine name of sheet
    n <- as.character(glue::glue(
      parameters$action_expectation_sheet$sheet_name,
      action_names = action_names[[i]]))
    ## return panel
    shiny::tabPanel(
     shiny::span(n, id = paste0("action_", i, "_panel")),
     rhandsontable::rHandsontableOutput(paste0("action_", i, "_widget")))
  }))

  # create panels
  panels <- do.call(
    shiny::tabsetPanel, append(list(id = "data_tabset_ui"), panels)
  )

  # return result
  do.call(
    shiny::tagList,
    append(
      list(
        panels,
        shinyBS::bsTooltip(
          "site_data_panel",
          "Table containing data for each site"
        ),
        shinyBS::bsTooltip(
          "site_status_panel",
          paste(
            "Table indicating which actions can potentially be",
            "implemented within each site"
          )
        ),
        shinyBS::bsTooltip(
          "feature_data_panel",
          "Table containing data for each feature"
        )
      ),
      lapply(seq_along(action_names), function(i) {
        shinyBS::bsTooltip(
          paste0("action_", i, "_panel"),
          paste0(
            "Table containing the expected amount of each feature within each ",
            "site if the \"", action_names[[i]], "\" action were implemented ",
            "within the site"
          )
        )
      })
    )
  )
}
