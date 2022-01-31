context("read_builtin_project")

test_that("simulated data (shapefile)", {
  # create object
  f <- system.file(
    "extdata", "projects", "simulated-data", "simulated-data.yaml",
    package = "whattodo"
  )
  parameters <- whatdataio::read_data_configuration()
  x <- read_builtin_project(path = f, parameters = parameters)
  # tests
  expect_is(x$name, "character")
  expect_is(x$author_name, "character")
  expect_is(x$author_email, "character")
  expect_is(x$project, "Project")
})

test_that("test data (shapefile)", {
  # create object
  f <- system.file(
    "extdata", "projects", "test-data", "test-data.yaml",
    package = "whattodo"
  )
  parameters <- whatdataio::read_data_configuration()
  x <- read_builtin_project(path = f, parameters = parameters)
  # tests
  expect_is(x$name, "character")
  expect_is(x$author_name, "character")
  expect_is(x$author_email, "character")
  expect_is(x$project, "Project")
})
