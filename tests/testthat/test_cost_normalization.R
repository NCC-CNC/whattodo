context("utils_normalize_data")

# Tests for max_cost function
test_that("max_cost handles regular positive values correctly", {
  df <- tibble::tibble(
    ID = 1:3,
    CostA = c(1234567, 987654321, 54321),
    CostB = c(10000000, 2500, 99999999)
  )
  expect_equal(max_cost(df), 987654321)
})

test_that("max_cost handles negative values correctly", {
  df_neg <- tibble::tibble(
    ID = 1:3,
    CostA = c(-1234567, -987654321, -54321),
    CostB = c(-10000000, -2500, -99999999)
  )
  expect_equal(max_cost(df_neg), -2500)
})

test_that("max_cost handles NA values correctly", {
  df_na <- tibble::tibble(
    ID = 1:3,
    CostA = c(1234567, NA, 54321),
    CostB = c(10000000, 2500, NA)
  )
  expect_equal(max_cost(df_na), 10000000)
})

test_that("max_cost works with custom column prefixes", {
  df_prefix <- tibble::tibble(
    ID = 1:3,
    ExpenseA = c(1234567, 987654321, 54321),
    ExpenseB = c(10000000, 2500, 99999999),
    CostC = c(5000000, 6000000, 7000000)
  )
  expect_equal(max_cost(df_prefix, col_starts_with = "Expense"), 987654321)
})

test_that("max_cost raises error when no matching columns exist", {
  # Create a tibble with no columns matching the default "Cost" prefix
  df_no_match <- tibble::tibble(
    ID = 1:3,
    ValueA = c(1234567, 987654321, 54321),
    ValueB = c(10000000, 2500, 99999999),
    Price = c(5000, 6000, 7000)
  )
  
  # Test with default prefix "Cost"
  expect_error(
    max_cost(df_no_match),
    "No columns starting with 'Cost' found in the input data."
  )
})

# Tests for num_whole_digits function
test_that("num_whole_digits works with positive integers", {
  expect_equal(num_whole_digits(12345), 5)
  expect_equal(num_whole_digits(1), 1)
  expect_equal(num_whole_digits(1000000), 7)
})

test_that("num_whole_digits works with decimal numbers", {
  expect_equal(num_whole_digits(0.1234), 1)  # Should be 1 for the '0'
  expect_equal(num_whole_digits(123.456), 3)
})

test_that("num_whole_digits works with negative numbers", {
  expect_equal(num_whole_digits(-12345), 5)
  expect_equal(num_whole_digits(-0.1234), 1)
})

test_that("num_whole_digits handles scientific notation correctly", {
  expect_equal(num_whole_digits(1e6), 7)
  expect_equal(num_whole_digits(1.23e4), 5)
})

test_that("num_whole_digits handles zero correctly", {
  expect_equal(num_whole_digits(0), 1)
})

test_that("num_whole_digits works with vector input", {
  expect_equal(num_whole_digits(c(12, 345, 6789)), c(2, 3, 4))
})

# Tests for normalize_cost_columns function
test_that("normalize_cost_columns correctly normalizes large values", {
  df_large <- tibble::tibble(
    ID = 1:3,
    CostA = c(1e7, 500000, 2000000),
    CostB = c(2500, 999999, 12345678),
    Value = c(10, 20, 30)
  )
  result_large <- normalize_cost_columns(df_large)
  
  # Check that the result is not NULL
  expect_false(is.null(result_large))
  
  # Check that the maximum value now has at most 6 whole digits
  expect_true(max_cost(result_large) < 1000000)
})

test_that("normalize_cost_columns preserves non-cost columns", {
  df_large <- tibble::tibble(
    ID = 1:3,
    CostA = c(1e7, 500000, 2000000),
    CostB = c(2500, 999999, 12345678),
    Value = c(10, 20, 30)
  )
  result_large <- normalize_cost_columns(df_large)
  
  # Check that non-cost columns remain unchanged
  expect_equal(result_large$ID, df_large$ID)
  expect_equal(result_large$Value, df_large$Value)
})

test_that("normalize_cost_columns scales values correctly", {
  df_large <- tibble::tibble(
    ID = 1:3,
    CostA = c(1e7, 500000, 2000000),
    CostB = c(2500, 999999, 12345678),
    Value = c(10, 20, 30)
  )
  
  # Calculate the expected divisor using the same logic as normalize_cost_columns
  max_value <- max(df_large$CostA, df_large$CostB, na.rm = TRUE)
  num_digits <- nchar(format(trunc(abs(max_value)), scientific = FALSE, trim = TRUE))
  expected_divisor <- 10^(num_digits - 6)
  
  result_large <- normalize_cost_columns(df_large)
  
  # Check that cost columns are scaled properly using the calculated divisor
  expect_equal(result_large$CostA[1], df_large$CostA[1] / expected_divisor)
  expect_equal(result_large$CostB[3], df_large$CostB[3] / expected_divisor)
  
  # Additional check to verify that the resulting values have at most 6 digits
  expect_true(num_whole_digits(max_cost(result_large)) <= 6)
})

test_that("normalize_cost_columns returns NULL when no normalization is needed", {
  df_small <- tibble::tibble(
    ID = 1:3,
    CostA = c(123456, 500000, 200000),
    CostB = c(2500, 999999, 123456),
    Value = c(10, 20, 30)
  )
  result_small <- normalize_cost_columns(df_small)
  
  # Check that the result is NULL
  expect_null(result_small)
})

test_that("normalize_cost_columns works with custom column prefixes", {
  df_prefix <- tibble::tibble(
    ID = 1:3,
    ExpenseA = c(1e7, 500000, 2000000),
    ExpenseB = c(2500, 999999, 12345678),
    CostC = c(5000000, 6000000, 7000000)  # This should be ignored
  )
  result_prefix <- normalize_cost_columns(df_prefix, col_starts_with = "Expense")
  
  # Check that only Expense columns are normalized
  expect_false(is.null(result_prefix))
  expect_equal(result_prefix$CostC, df_prefix$CostC)  # Should remain unchanged
  
  # Check that Expense columns are normalized
  # Use max_cost function to get the maximum value from expense columns
  expect_true(max_cost(result_prefix, col_starts_with = "Expense") < 1000000)
})

test_that("normalize_cost_columns handles NA values correctly", {
  df_na <- tibble::tibble(
    ID = 1:3,
    CostA = c(1e7, NA, 2000000),
    CostB = c(2500, 999999, NA)
  )
  result_na <- normalize_cost_columns(df_na)
  
  # Check that NA values are handled correctly
  expect_false(is.null(result_na))
  expect_true(is.na(result_na$CostA[2]))
  expect_true(is.na(result_na$CostB[3]))
})
