# Initialization
## load packages
devtools::load_all()
library(whatdataio)
library(sf)
library(dplyr)
library(tibble)

## define variables
shp_path <- "inst/extdata/projects/simulated-data/simulated-data.shp"
output_path <- "inst/extdata/projects/simulated-data/simulated-data.xlsx"

## load parameters
parameters <- whatdataio::read_data_configuration()

## define data
site_data <- tibble::tribble(
  ~id, ~description,
  "Johnston's Pond", "Pond in Nova Scotia",
  "Lobster Bay", "Bay in Nova Scotia",
  "Port Joli", "Ecologically important coastal area",
  "Round Bay", "Bay community in the Canadian province of Nova Scotia"
)
feature_data <- tibble::tribble(
  ~id, ~description,
  "Salt Marsh", "Coastal ecosystem in the upper coastal intertidal zone",
  "ACPF", "Atlantic Coastal Plain Flora are a group of 94 herbaceous plants",
  "Freshwater Wetland", "Non-tidal, non-forested marsh wetland that contains freshwater, and is frequently flooded"
)
action_data <- tibble::tribble(
  ~id, ~description,
  "Maintain", "Maintenance actions",
  "Restore", "Restoration actions to improve ecological integrity",
  "Signage", "Add signs to informing of local catch quotas"
)

# Preliminary processing
## load spatial data
spatial_data <- sf::read_sf(shp_path)
assertthat::assert_that(
  identical(spatial_data[[1]], site_data$id),
  msg = "site identifiers don't match"
)

# Main processing
## calculate longitudes and latitudes for data
site_coords <-
  spatial_data %>%
  sf::st_transform(4326) %>%
  sf::st_centroid() %>%
  sf::as_Spatial() %>%
  slot("coords") %>%
  as.data.frame() %>%
  setNames(c("lon", "lat")) %>%
  tibble::as_tibble()

## create workbook to initialize data
f1 <- tempfile(fileext = ".xlsx")
openxlsx::saveWorkbook(
  wb = whatdataio::create_template_workbook(
    site_ids = site_data$id,
    site_descriptions = site_data$description,
    feature_ids = feature_data$id,
    feature_descriptions = feature_data$description,
    action_ids = action_data$id,
    action_descriptions = action_data$description,
    parameters = parameters,
    site_longitudes = site_coords$lon,
    site_latitudes = site_coords$lat
  ),
  file = f1,
  overwrite = TRUE,
  returnValue = FALSE
)

## import workbook
wb <- read_spreadsheet_data(f1, parameters)

## assign values to data
### site data
wb$site_data[[4]] <- sample(action_data$id, nrow(site_data), replace = TRUE)
for (i in seq_len(nrow(action_data))) {
  wb$site_data[[4 + i]] <- round(runif(nrow(site_data), i * 100, i * 200))
}

### feature data
wb$feature_data[[2]] <- round(runif(nrow(feature_data), 1, 100))
wb$feature_data[[3]] <- round(runif(nrow(feature_data), 1, 100))

### consequence data
for (i in seq_len(nrow(action_data))) {
  for (j in seq_len(nrow(feature_data))) {
    wb$consequence_data[[i]][[j + 1]] <- round(
      runif(nrow(site_data), 1, 100)
    )
  }
}

# Exports
## save workbook
openxlsx::saveWorkbook(
  create_project_workbook(
    site_ids = site_data$id,
    site_descriptions = site_data$description,
    feature_ids = feature_data$id,
    feature_descriptions = feature_data$description,
    action_ids = action_data$id,
    action_descriptions = action_data$description,
    site_data = wb$site_data,
    feasibility_data = wb$feasibility_data,
    feature_data = wb$feature_data,
    consequence_data = wb$consequence_data,
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
