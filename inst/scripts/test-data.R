# Initialization
## load packages
devtools::load_all()
library(whatdataio)
library(sf)
library(dplyr)
library(tibble)

## define variables
n_actions <- 5
n_features <- 10
n_sites <- 50
output_dir <- "inst/extdata/projects/test-data"

## load data
parameters <- whatdataio::read_data_configuration()

# Main processing
## simulate project data
d <- whatdataio::simulate_project_data(
  n_sites = n_sites,
  n_features = n_features,
  n_actions = n_actions,
  parameters = parameters
)

## simulate site locations
### create study area
study_area_data <-
  system.file("shape/nc.shp", package = "sf") %>%
  sf::read_sf() %>%
  sf::st_union() %>%
  sf::st_cast("POLYGON") %>%
  sf::st_sf() %>%
  filter(sf::st_area(.) == max(sf::st_area(.))) %>%
  sf::st_transform(4326) %>%
  sf::st_make_valid()

### create grid cells for sites
grid_data <-
  study_area_data %>%
  sf::st_make_grid(n = 20) %>%
  sf::st_set_crs(NA) %>%
  sf::st_intersection(x = sf::st_set_crs(study_area_data, NA)) %>%
  sf::st_set_crs(4326)

### sample sites based on grid cells
site_data <-
  grid_data[sample.int(nrow(grid_data), n_sites), , drop = FALSE]

# Exports
## save workbook
output_path <- paste0(output_dir, "/test-data.xslx")
openxlsx::saveWorkbook(
  create_project_workbook(
    site_ids = d$site_ids,
    site_descriptions = d$site_descriptions,
    feature_ids = d$feature_ids,
    feature_descriptions = d$feature_descriptions,
    action_ids = d$action_ids,
    action_descriptions = d$action_descriptions,
    site_data = d$site_data,
    feasibility_data = d$feasibility_data,
    feature_data = d$feature_data,
    consequence_data = d$consequence_data,
    parameters = parameters
  ),
  file = output_path,
  overwrite = TRUE,
  returnValue = FALSE
)
assertthat::assert_that(
  file.exists(output_path),
  msg = "failed to save workbook"
)

## save shapefile
sf::write_sf(
  site_data,
  paste0(output_dir, "/test-data.shp"),
  overwrite = TRUE
)
