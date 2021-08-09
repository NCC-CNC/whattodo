context("verification")

test_that("has_unique_values", {
  expect_true(has_unique_values(letters[1:3]))
  expect_true(has_unique_values(letters[c(1, NA, 2, 3)]))
  expect_false(has_unique_values(letters[c(1, 2, 1)]))
  expect_false(has_unique_values(letters[c(1, NA, 2, 1)]))

})

test_that("has_valid_values", {
  expect_true(has_valid_values(c("1", "2")))
  expect_false(has_valid_values(c()))
  expect_false(has_valid_values(c("1", NA)))
  expect_false(has_valid_values(c("1", "")))
})
