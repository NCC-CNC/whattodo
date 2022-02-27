context("is_valid_spreadsheet_file")

test_that("correctly identifies spreadsheets", {
  # simulated data
  f <- system.file(
    "extdata", "projects", "simulated-data", "simulated-data.xlsx",
    package = "whattodo"
  )
  expect_true(is_valid_spreadsheet_file(f))
  # test data
  f <- system.file(
    "extdata", "projects", "test-data", "test-data.xlsx",
    package = "whattodo"
  )
  expect_true(is_valid_spreadsheet_file(f))
})

test_that("correctly identifies non-spreadsheets", {
  # file that does not have xlsx extension
  f <- system.file(
    "extdata", "projects", "simulated-data", "simulated-data.yaml",
    package = "whattodo"
  )
  # file that has xlsx extension but is not actually a spreadsheet
  expect_is(is_valid_spreadsheet_file(f), "character")
  f <- tempfile(fileext = ".xsx")
  file.copy(
    from = system.file(
      "extdata", "projects", "simulated-data", "simulated-data.yaml",
      package = "whattodo"
    ),
    to = f
  )
  expect_is(is_valid_spreadsheet_file(f), "character")
  unlink(f, force = TRUE)
})
