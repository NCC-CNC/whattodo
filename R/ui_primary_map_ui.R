#' @include internal.R
NULL

#' Map user interface
#'
#' Create a user interface for visualizing the spatial data.
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
primary_map_ui <- function(site_names, feature_names, action_names,
                           parameters) {
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

  # return ui
  leaflet::leafletOutput("map_widget")
}
