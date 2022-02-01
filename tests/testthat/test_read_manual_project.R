context("read_manual_project")

test_that("no geometry data", {
  # create object
  f <- system.file(
    "extdata", "projects", "simulated-data", "simulated-data.xlsx",
    package = "whattodo"
  )
  parameters <- whatdataio::read_data_configuration()
  x <- read_manual_project(
    spreadsheet_path = f,
    parameters = parameters,
    author_name = "Greg",
    author_email = "greg@gmail.com"
  )
  # tests
  expect_is(x$name, "character")
  expect_equal(x$author_name, "Greg")
  expect_equal(x$author_email, "greg@gmail.com")
  expect_is(x$project, "Project")
})

test_that("shapefile", {
  # create object
  f1 <- system.file(
    "extdata", "projects", "simulated-data", "simulated-data.xlsx",
    package = "whattodo"
  )
  f2 <- system.file(
    "extdata", "projects", "simulated-data", "simulated-data.shp",
    package = "whattodo"
  )
  parameters <- whatdataio::read_data_configuration()
  x <- read_manual_project(
    spreadsheet_path = f1,
    parameters = parameters,
    spatial_path = f2,
    author_name = "Greg",
    author_email = "greg@gmail.com"
  )
  # tests
  expect_is(x$name, "character")
  expect_equal(x$author_name, "Greg")
  expect_equal(x$author_email, "greg@gmail.com")
  expect_is(x$project, "Project")
})

test_that("zipfile", {
  # create object
  f1 <- system.file(
    "extdata", "projects", "simulated-data", "simulated-data.xlsx",
    package = "whattodo"
  )
  f2 <- tempfile(fileext = ".zip")
  withr::with_dir(
    system.file("extdata", "projects", "simulated-data", package = "whattodo"),
    utils::zip(
      zipfile = f2,
      files = paste0("simulated-data", c(".shp", ".shx", ".prj", ".dbf")),
      flags = c("-r9X", "-qq")
    )
  )
  parameters <- whatdataio::read_data_configuration()
  x <- read_manual_project(
    spreadsheet_path = f1,
    parameters = parameters,
    spatial_path = f2,
    author_name = "Greg",
    author_email = "greg@gmail.com"
  )
  # tests
  expect_is(x$name, "character")
  expect_equal(x$author_name, "Greg")
  expect_equal(x$author_email, "greg@gmail.com")
  expect_is(x$project, "Project")
  # clean up
  rm(f2)
})
