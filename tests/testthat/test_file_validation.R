context("file validation")

test_that("is_valid_configuration_file", {
  # find path
  f <- system.file(
    "extdata", "projects", "simulated-data", "simulated-data.yaml",
    package = "whattodo"
  )
  # tests
  expect_true(is_valid_configuration_file(f))
})

test_that("is_valid_shapefile_file", {
  # find path
  f <- system.file(
    "extdata", "projects", "simulated-data",
    paste0("simulated-data", c(".shp", ".shx", ".dbf", ".prj")),
    package = "whattodo"
  )
  # tests
  expect_true(is_valid_shapefile_file(f))
})
