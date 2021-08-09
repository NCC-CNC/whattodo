context("has_valid_spreadsheet_data")

test_that("expected result", {
  expect_true(has_valid_spreadsheet_data(
    list(site_data = data.frame(a = c(1, 2, 3)))
  ))
  expect_true(has_valid_spreadsheet_data(
    list(site_data = data.frame(a = c("a", "b", "c")))
  ))
  expect_false(has_valid_spreadsheet_data(
    list(site_data = data.frame(a = c(1, 2, NA_real_)))
  ))
  expect_false(has_valid_spreadsheet_data(
    list(site_data = data.frame(a = c("a", "b", NA)))
  ))
})
