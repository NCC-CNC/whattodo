context("file validation")

test_that("is_valid_configuration_file", {
  # find path
  f <- system.file("extdata", "projects", "canada", "canada.yaml")
  # tests
  expect_true(is_valid_configuration_file(f))

})

test_that("is_valid_shapefile_file", {
  # find path
  f <- system.file("extdata", "projects", "canada", "canada.shp")
  # tests
  expect_true(is_valid_shapefile_file(f))
})
