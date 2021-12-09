#' @include internal.R
NULL

#' Is valid configuration file?
#'
#' Verify if a file path corresponds to a valid configuration file.
#'
#' @param x `character` file path.
#'
#' @return `logical` value indicating validity.
#'
#' @export
is_valid_configuration_file <- function(x) {
  # assert argument is valid
  assertthat::assert_that(assertthat::is.string(x))

  # check if YAML file based on file name
  if (!endsWith(x, ".yaml")) {
    return("Error: file must have a \".yaml\" extension")
  }

  # try importing file
  f <- try(yaml::read_yaml(x), silent = TRUE)

  # check if can be parsed as valid YAML file
  if (inherits(f, "try-error")) {
    return("Error: not valid file format")
  }

  # check if YAML file contains correct keys
  key_names <- c("name")
  if (!all(c(key_names) %in% names(f))) {
    return("Error: YAML file is missing data")
  }

  # return success
  TRUE
}

#' Is valid shapefile file?
#'
#' Verify if a file path corresponds to a valid ESRI Shapefile.
#'
#' @inheritParams is_valid_configuration_file
#'
#' @inherit is_valid_configuration_file return
#'
#' @export
is_valid_shapefile_file <- function(x) {
  # assert argument is valid
  assertthat::assert_that(is.character(x))

  # check if contains recognized extensions
  if (any(endsWith(x, ".shp"))) {
    ## extract shp file
    p <- x[endsWith(x, ".shp")]

    ## verify that shapefile has all components
    if (!any(endsWith(x, ".shx"))) {
      return("Error: missing \".shx\" file")
    }
    if (!any(endsWith(x, ".dbf"))) {
      return("Error: missing \".dbf\" file")
    }
    if (!any(endsWith(x, ".prj"))) {
      return("Error: missing \".prj\" file")
    }

    ## verify valid shapefile
    ## note that only first couple of rows are imported to reduce run time
    l <- try(sf::st_layers(p)$name[[1]], silent = TRUE)
    if (inherits(l, "try-error")) {
      return("Error: not valid ESRI Shapefile format")
    }
    qu <- paste0("SELECT * FROM \"", l, "\" WHERE FID <= 5")
    f <- suppressWarnings(
      try(sf::read_sf(dsn = p, layer = l, query = qu), silent = TRUE)
    )
    if (inherits(f, "try-error")) {
      return("Error: not valid ESRI Shapefile format")
    }
  } else {
    ## throw error because file is not ESRI Shapefile
    return("Error: not a valid ESRI Shapefile file format.")
  }

  # return success
  TRUE
}
