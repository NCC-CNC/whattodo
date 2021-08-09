#' @include internal.R
NULL

#' Sidebar user interface
#'
#' Create a sidebar user interface for generating prioritizations.
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
primary_sidebar_ui <- function(
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

  # create ui
  shiny::fluidRow(
    shinyBS::bsCollapse(
      id = "collapse",
      multiple = TRUE,
      open = c(
        "Instructions (click to minimize/maximize)",
        "Settings (click to minimize/maximize)"
      ),
      shinyBS::bsCollapsePanel(
        title = "Instructions (click to minimize/maximize)",
        shiny::helpText(
          "Thanks for uploading your data! You can now generate optimized management plans by clicking the \"Generate prioritization!\" button. After clicking this button, the map will display which management actions are allocated to which sites in an optimized management plan. To further examine the plan, please click on the \"Results\" tab and see the associated tables (e.g. \"Summary  results\"). You can also change the input data without having to upload the Excel Spreadsheet by clicking on the \"Data\" tab, selecting a sheet to modify (e.g. \"Feature data\"), selecting a cell (e.g. first row in the \"Target threshold amount\" column), typing in a new value, and pressing enter to confirm the new value. If you change the input data and click the \"Generate prioritization!\" button, the app will use the updated data to generate a new optimized management plan. To download a copy of all the input data and results tables on-screen, you can click on the \"Download results\" button. You can also customize the optimization process by clicking on the \"Settings\" panel below (e.g. specify a maximum budget for prioritized actions)."
        ),
        shiny::helpText(
          "Please note that feature weights (i.e. values in the \"Relative importance (weights)\" column of \"Feature data\") are only used to inform prioritizations if a budget is specified. As such, the feature weights will not have any influence over the results if a budget is not specified."
        )
      ),
      shinyBS::bsCollapsePanel(
        title = "Settings (click to minimize/maximize)",
        shinyBS::tipify(
          el = shiny::checkboxInput(
            inputId = "problem_widget",
            "Use a budget?",
            value = FALSE
          ),
          title = paste(
            "Note that feature weights will only affect results if a budget",
            "is specified"
          )
        ),
        shiny::conditionalPanel(
          condition = "input.problem_widget == 1",
          shiny::numericInput(inputId = "budget_widget", "Budget", value = 0)
        )
      )
    ),
    shiny::div(
      class = "divCenter",
      shinyBS::bsButton(
        inputId = "solution_btn",
        label = "Generate prioritization!",
        style = "primary"
      )
    ),
    shiny::br(),
    shiny::div(
      shiny::div(
        id = "export_btn_div",
        class = "divShrink",
        shiny::downloadButton("export_btn", label = "Download results")
      ),
      class = "divCenter"
    ),
    shiny::div(
      shinyBS::bsTooltip(
        id = "solution_btn",
        placement = "right",
        title = "Click this button to identify a priority action for each site."
      ),
      shinyBS::bsTooltip(
        id = "export_btn",
        placement = "right",
        title = paste(
           "Click this button to save a snapshot of the",
           "input data and results to your computer"
         )
       )
    )
  )
}
