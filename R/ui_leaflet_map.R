#' @include internal.R
NULL

#' Leaflet map
#'
#' Create a leaflet map with customized default settings.
#'
#' @param sidebar_id `character` vector containing HTML identifiers for
#' the sidebar.
#'
#' @return [leaflet::leaflet()] object.
#'
#' @export
leaflet_map <- function(sidebar_id) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(sidebar_id),
    assertthat::noNA(sidebar_id)
  )

  # prepare JS code for buttons
  home_js <- paste0(
    "function(btn, map) {",
    "Shiny.setInputValue(\"home_button\", Math.random());",
    "}"
  )
  print_js <- paste0(
    "function(btn, map) {",
    "Shiny.setInputValue(\"print_button\", Math.random());",
    "}"
  )
  help_js <- paste0(
    "function(btn, map) {",
    "Shiny.setInputValue(\"help_button\", Math.random());",
    "}"
  )

  # create map
  map <-
    ## initialize leaflet map
    leaflet::leaflet() %>%
    ## add basemaps
    leaflet::addProviderTiles(
      leaflet::providers$Esri.WorldImagery,
      group = "Satellite"
    ) %>%
    leaflet::addProviderTiles(
      leaflet::providers$Esri.WorldStreetMap,
      group = "Street view"
    ) %>%
    leaflet::addProviderTiles(
      leaflet::providers$CartoDB.DarkMatter,
      group = "Monochrome"
    ) %>%
    leaflet::addProviderTiles(
      leaflet::providers$CartoDB.Positron,
      group = "Gray scale"
    ) %>%
    ## specify default view window
    leaflet::flyToBounds(
      -165, -30, 165, 60
    ) %>%
    ## set map bounds
    leaflet::setMaxBounds(
      -200, -100, 200, 100
    ) %>%
    ## add home button
    leaflet::addEasyButton(
      leaflet::easyButton(
        title = "Zoom to home",
        icon = shiny::icon("home"),
        position = "topleft",
        onClick = htmlwidgets::JS(home_js)
      )
    ) %>%
    ## add help button
    leaflet::addEasyButton(
      leaflet::easyButton(
        title = "Help",
        icon = shiny::icon("question"),
        position = "topleft",
        onClick = htmlwidgets::JS(help_js)
      )
    ) %>%
    ## add screenshot button
    leaflet.extras2::addEasyprint(
      options = leaflet.extras2::easyprintOptions(
        exportOnly = TRUE,
        hidden = TRUE
      )
    ) %>%
    leaflet::addEasyButton(
      leaflet::easyButton(
        title = "Take screenshot",
        icon = shiny::icon("print"),
        position = "topleft",
        onClick = htmlwidgets::JS(print_js)
      )
    ) %>%
    ## add basemap controls
    leaflet::addLayersControl(
      baseGroups = c("Satellite", "Street view", "Monochrome", "Gray scale"),
      options = leaflet::layersControlOptions(collapsed = TRUE),
      position = "topleft"
    ) %>%
    ## add history buttons
    leaflet.extras2::addHistory(
      options = leaflet.extras2::historyOptions(position = "topleft")
    ) %>%
    ## add scale bar
    leaflet::addScaleBar(
      position = "bottomright"
    )

  # add sidebars
  map <- leaflet.extras2::addSidebar(
    map,
    id =  sidebar_id,
    options = list(position = "left", fit = FALSE)
  )

  # remove outdated font awesome dependency
  idx <- vapply(
    map$dependencies,
    FUN.VALUE = logical(1),
    function(x) x$name == "fontawesome" && x$version == "4.7.0"
  )
  map$dependencies <- map$dependencies[which(!idx)]

  # add custom JS code to make Bootstrap tooltips
  map <- htmlwidgets::onRender(map, "
  function(el, x) {
     document
     .querySelectorAll('.leaflet-top *[title]:not([title=\"\"]')
     .forEach((x) => {
       $(x).tooltip({
         trigger: 'hover',
         placement: 'top',
         container: 'body',
         placement: 'bottom'
      });
     });
  }")

  # return result
  map
}
