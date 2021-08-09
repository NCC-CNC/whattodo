#' @include internal.R
NULL

#' Initialize map
#'
#' Create a (\pkg{leaflet}) map. This map is shown by default
#' after data are loaded into the application, before any solutions are
#' generated.
#'
#' @param site_bbox `bbox` bounding box for site data.
#'
#' @param site_spatial_data `sf` spatial data for sites.
#'
#' @param parameters `list` parameters for interpreting the data.
#'   Defaults to configuration parameters bundled with the application.
#'
#' @return A [leaflet::leaflet()] map object.
#'
#' @export
initialize_map <- function(site_bbox, site_spatial_data, parameters) {
  # assert that arguments are valid
  ## validate classes
  assertthat::assert_that(
    inherits(site_bbox, "bbox"),
    inherits(site_spatial_data, c("sf", "NULL")),
    is.list(parameters))

  # expand viewport by 10%
  site_bbox <- as.list(site_bbox)
  site_bbox2 <- list()
  site_bbox2$xmin <- site_bbox$xmin - (0.1 * (site_bbox$xmax - site_bbox$xmin))
  site_bbox2$xmax <- site_bbox$xmax + (0.1 * (site_bbox$xmax - site_bbox$xmin))
  site_bbox2$ymin <- site_bbox$ymin - (0.1 * (site_bbox$ymax - site_bbox$ymin))
  site_bbox2$ymax <- site_bbox$ymax + (0.1 * (site_bbox$ymax - site_bbox$ymin))

  # preliminary processing
  ## prepare JS code
  fly_to_sites_js <- paste0(
    "function(btn, map){ map.flyToBounds([",
    "[", site_bbox2$ymin, ", ", site_bbox2$xmin, "],",
    "[", site_bbox2$ymax, ", ", site_bbox2$xmax, "]]);}"
  )

  # make map
  ## initialize map
  map <-
    leaflet::leaflet() %>%
     leaflet::addMiniMap() %>%
    leaflet::flyToBounds(
      lng1 = site_bbox2$xmin,
      lat1 = site_bbox2$ymin,
      lng2 = site_bbox2$xmax,
      lat2 = site_bbox2$ymax
    ) %>%
    leaflet::addScaleBar(
      position = "bottomright"
    ) %>%
    leaflet::addEasyButton(
      leaflet::easyButton(
        icon = "fa-crosshairs", title = "Zoom to sites",
        onClick = htmlwidgets::JS(fly_to_sites_js)
      )
    ) %>%
    leaflet::addLayersControl(
      baseGroups = parameters$map$basemap_name, position = "topright"
    )

  ## add basemaps
  for (b in seq_along(parameters$map$basemap_name)) {
    map <- leaflet::addProviderTiles(
      map, parameters$map$basemap_key[b],
      group = parameters$map$basemap_name[b]
    )
  }

  ## add sites to map
  if (!is.null(site_spatial_data)) {
    map <- reset_map(map, site_spatial_data, parameters, NULL)
  }

  # return result
  map
}

#' Update map
#'
#' @param map `leaflet` map object from `leafletProxy`.
#'
#' @param site_results_data `data.frame` object with results from
#'   prioritization.
#'
#' @param action_expectation_data `list` object with action expectation data.
#'
#' @param action_colors `character` colors to use for rendering prioritizations.
#'
#' @param legend_title `character` title of legend.
#'
#' @inheritParams initialize_map
#' @inheritParams format_pu_data
#'
#' @inherit initialize_map return
#'
#' @export
update_map <- function(
  map, feature_names, site_spatial_data, site_results_data,
  action_expectation_data, action_colors, parameters,
  legend_title = NULL) {
  # assert valid arguments
  assertthat::assert_that(
    inherits(map, c("leaflet", "leaflet_proxy")),
    inherits(site_spatial_data, "sf"),
    inherits(action_colors[[1]], "character"),
    inherits(action_colors, "matrix"),
    assertthat::noNA(c(action_colors)),
    is.character(feature_names),
    assertthat::noNA(feature_names),
    inherits(legend_title, c("character", "NULL")),
    is.list(parameters))

  # determine plotting colors
  idx <- match(
    site_results_data[[parameters$site_results_sheet$action_header]],
    colnames(action_colors)
  )
  assertthat::assert_that(assertthat::noNA(idx))
  col <- action_colors[, idx, drop = FALSE]

  # create table with expectation results
  tbl <- tibble::tibble(name = site_spatial_data$name)
  names(tbl) <- parameters$site_results_sheet$name_header
  site_results_data[, 1, drop = FALSE]
  m <- matrix(NA, nrow = nrow(site_spatial_data), ncol = length(feature_names))
  colnames(m) <- feature_names
  for (i in seq_len(nrow(m))) {
    m[i, ] <- c(as.matrix(action_expectation_data[[idx[[i]]]][i, -1]))
  }
  tbl <- dplyr::bind_cols(tbl, tibble::as_tibble(as.data.frame(m)))

  # update legend
  map <- leaflet::removeControl(map, layerId = "legend_widget")
  map <- leaflet::addLegend(
    map,
    layerId = "legend_widget",
    position = "bottomleft",
    colors = unname(action_colors[2, ]),
    labels = colnames(action_colors), opacity = 1,
    title = "Priority actions"
  )

  # update site data
  map <- add_to_map(
    map = map,
    x = site_spatial_data,
    id = site_spatial_data$id,
    col = col,
    popup = tbl
  )

  # return map
  map
}

#' Reset map
#'
#' @inheritParams initialize_map
#' @inheritParams update_map
#'
#' @inherit initialize_map return
#'
#' @export
reset_map <- function(
  map, site_spatial_data, parameters, legend_title = NULL) {
  # assert valid arguments
  assertthat::assert_that(
    inherits(map, c("leaflet", "leaflet_proxy")),
    inherits(site_spatial_data, "sf"),
    inherits(legend_title, c("character", "NULL")),
    is.list(parameters))

  # define default color
  col <- default_color_matrix(nrow(site_spatial_data), parameters)

  # prepare popup table
  tbl <- tibble::tibble(name = site_spatial_data$name)
  names(tbl) <- parameters$site_results_sheet$name_header

  # add legend
  map <- leaflet::removeControl(map, layerId = "legend_widget")
  map <- leaflet::addLegend(
    map,
    layerId = "legend_widget",
    position = "bottomleft",
    colors = col[2, 1],
    labels = colnames(col)[[1]], opacity = 1)

  # update site data
  map <- add_to_map(
    map = map,
    x = site_spatial_data,
    id = site_spatial_data$id,
    col = col,
    popup = tbl
  )

  # return map
  map
}

#' Add spatial data to map
#'
#' @param  map `leaflet` map.
#'
#' @param x `sf` spatial data.
#'
#' @param id `character` vector of identifiers for each geometry.
#'
#' @param col `character` vector of colors.
#'
#' @param popup `data.frame` with popup data.
#'
#' @return An updated [leaflet::leaflet()] map object.
#'
#' @export
add_to_map <- function(map, x, id, col, popup) {
  # assert valid arguments
  assertthat::assert_that(
    inherits(map, c("leaflet", "leaflet_proxy")),
    inherits(x, "sf"),
    is.character(id),
    assertthat::noNA(id),
    length(id) == nrow(x),
    identical(anyDuplicated(id), 0L),
    inherits(col[[1]], "character"),
    inherits(col, "matrix"),
    assertthat::noNA(c(col)),
    ncol(col) == nrow(x),
    inherits(popup, "data.frame"),
    nrow(popup) == nrow(x))

  # preliminary processing
  ## determine spatial data type
  type <- st_geometry_data_type(x)

  ## coerce popup to data.frame
  ## (avoid incompatibility issues with tibble)
  popup <- as.data.frame(popup, stringsAsFactors = FALSE)

  # main processing
  ## depends on the type of geometries used for representing sites..
  if (identical(type, "POINT")) {
    ### create markers
    markers <- lapply(seq_len(nrow(x)), function(i) {
      leaflet::makeAwesomeIcon(
        icon = "circle", markerColor = unname(col[1, i]))
    })
    ### update if sites are points
    map <- leaflet::removeMarker(map, layerId = id)
    for (i in seq_len(nrow(x))) {
      map <- leaflet::addAwesomeMarkers(
        map,
        data = x[i, , drop = FALSE],
        layerId = id[i], icon = markers[[i]],
        popup = leafpop::popupTable(
          popup[i, , drop = FALSE],
          row.numbers = FALSE,
          feature.id = FALSE
        )
      )
    }
  } else if (
    identical(type, "POLYGON") || identical(type, "MULTIPOLYGON")) {
    ### update if sites are polygons
    map <- leaflet::removeShape(map, layerId = id)
    map <- leaflet::addPolygons(
      map,
      data = x,
      layerId = id,
      color = unname(col[2, ]),
      fillColor = unname(col[2, ]),
      popup = leafpop::popupTable(
        popup,
        row.numbers = FALSE,
        feature.id = FALSE
      )
    )
  } else if (
    identical(type, "LINESTRING") || identical(type, "MULTILINESTRING")) {
      ### update if sites are lines
    map <- leaflet::removeShape(map, layerId = id)
    map <- leaflet::addPolylines(
      map,
      data = x,
      layerId = id,
      color = unname(col[2, ]),
      fillColor = unname(col[2, ]),
      popup = leafpop::popupTable(
        popup,
        row.numbers = FALSE,
        feature.id = FALSE
      )
    )
  } else {
    ### add data using GeoJSON format as a last resort
    map_json <-
      as.character(geojsonsf::sfc_geojson(sf::st_geometry(x)))
    map <- leaflet::removeGeoJSON(map, layerId = id)
    map <- leaflet::addGeoJSON(
      map,
      map_json,
      fill = TRUE,
      layerId = id,
      color = unname(col[2, ]),
      fillColor = unname(col[2, ])
    )
  }

  # return updated map
  map
}
