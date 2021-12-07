context("read_builtin_project")

test_that("raw shapefile", {
  # create object
  f <- system.file(
    "extdata", "projects", "canada", "canada.yaml", package = "whattodo"
  )
  parameters <- whatdataio::read_data_configuration()
  x <- read_builtin_project(path = f, parameters = parameters)
  # tests
  expect_is(x, "Project")
})

test_that("zipped shapefile", {
  # TODO
})
