context("utils_normalize_data")

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
  
  # Calculate max budget for testing
  max_budget <- max(df_large$CostA, df_large$CostB, na.rm = TRUE)
  
  result_large <- normalize_cost_columns(df_large, max_budget)
  
  # Check that the result is not NULL
  expect_false(is.null(result_large))
  
  # Check that the maximum value now has at most 6 whole digits
  expect_true(max(result_large$CostA, result_large$CostB, na.rm = TRUE) < 1000000)
})

test_that("normalize_cost_columns preserves non-cost columns", {
  df_large <- tibble::tibble(
    ID = 1:3,
    CostA = c(1e7, 500000, 2000000),
    CostB = c(2500, 999999, 12345678),
    Value = c(10, 20, 30)
  )
  
  # Calculate max budget for testing
  max_budget <- max(df_large$CostA, df_large$CostB, na.rm = TRUE)
  
  result_large <- normalize_cost_columns(df_large, max_budget)
  
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
  
  # Calculate max budget for testing
  max_budget <- max(df_large$CostA, df_large$CostB, na.rm = TRUE)
  
  # Calculate the expected divisor using the same logic as normalize_cost_columns
  num_digits <- nchar(format(trunc(abs(max_budget)), scientific = FALSE, trim = TRUE))
  expected_divisor <- 10^(num_digits - 6)
  
  result_large <- normalize_cost_columns(df_large, max_budget)
  
  # Check that cost columns are scaled properly using the calculated divisor
  expect_equal(result_large$CostA[1], df_large$CostA[1] / expected_divisor)
  expect_equal(result_large$CostB[3], df_large$CostB[3] / expected_divisor)
  
  # Additional check to verify that the resulting values have at most 6 digits
  expect_true(max(result_large$CostA, result_large$CostB, na.rm = TRUE) < 1000000)
})

test_that("normalize_cost_columns returns NULL when no normalization is needed", {
  df_small <- tibble::tibble(
    ID = 1:3,
    CostA = c(123456, 500000, 200000),
    CostB = c(2500, 999999, 123456),
    Value = c(10, 20, 30)
  )
  
  # Calculate max budget for testing
  max_budget <- max(df_small$CostA, df_small$CostB, na.rm = TRUE)
  
  result_small <- normalize_cost_columns(df_small, max_budget)
  
  # Check that the result is NULL when max_budget < 1000000
  expect_null(result_small)
})

test_that("normalize_cost_columns works with custom column prefixes", {
  df_prefix <- tibble::tibble(
    ID = 1:3,
    ExpenseA = c(1e7, 500000, 2000000),
    ExpenseB = c(2500, 999999, 12345678),
    CostC = c(5000000, 6000000, 7000000)  # This should be ignored
  )
  
  # Calculate max budget for testing (with the custom prefix)
  max_budget <- max(df_prefix$ExpenseA, df_prefix$ExpenseB, na.rm = TRUE)
  
  result_prefix <- normalize_cost_columns(df_prefix, max_budget, col_starts_with = "Expense")
  
  # Check that only Expense columns are normalized
  expect_false(is.null(result_prefix))
  expect_equal(result_prefix$CostC, df_prefix$CostC)  # Should remain unchanged
  
  # Check that Expense columns are normalized
  expect_true(max(result_prefix$ExpenseA, result_prefix$ExpenseB, na.rm = TRUE) < 1000000)
})

test_that("normalize_cost_columns handles NA values correctly", {
  df_na <- tibble::tibble(
    ID = 1:3,
    CostA = c(1e7, NA, 2000000),
    CostB = c(2500, 999999, NA)
  )
  
  # Calculate max budget for testing
  max_budget <- max(df_na$CostA, df_na$CostB, na.rm = TRUE)
  
  result_na <- normalize_cost_columns(df_na, max_budget)
  
  # Check that NA values are handled correctly
  expect_false(is.null(result_na))
  expect_true(is.na(result_na$CostA[2]))
  expect_true(is.na(result_na$CostB[3]))
})

test_that("normalize_cost_columns uses provided max_budget parameter correctly", {
  df <- tibble::tibble(
    ID = 1:3,
    CostA = c(1e5, 5e5, 2e5),
    CostB = c(2500, 9e5, 1e5)
  )
  
  # Provide a max_budget that's larger than any individual cost in the data
  max_budget <- 2e6
  
  result <- normalize_cost_columns(df, max_budget)
  
  # Check that normalization occurs based on the provided max_budget, not the actual max in the data
  expect_false(is.null(result))
  
  # Calculate the expected divisor based on the provided max_budget
  num_digits <- nchar(format(trunc(abs(max_budget)), scientific = FALSE, trim = TRUE))
  expected_divisor <- 10^(num_digits - 6)
  
  # Check that values are scaled according to the provided max_budget
  expect_equal(result$CostA[1], df$CostA[1] / expected_divisor)
})

# test normalize_budget function
test_that("normalize_budget correctly scales values when max_budget exceeds 1M", {
  # Test with a large max_budget value
  max_budget <- 25000000
  
  # Calculate the expected divisor
  num_digits <- nchar(format(trunc(abs(max_budget)), scientific = FALSE, trim = TRUE))
  expected_divisor <- 10^(num_digits - 6)
  
  # Various budget values to test
  budget1 <- 5000000
  budget2 <- 750000
  budget3 <- 12345678
  
  # Check that each value is scaled correctly
  expect_equal(normalize_budget(budget1, max_budget), budget1 / expected_divisor)
  expect_equal(normalize_budget(budget2, max_budget), budget2 / expected_divisor)
  expect_equal(normalize_budget(budget3, max_budget), budget3 / expected_divisor)
  
  # Check that the largest normalized value has at most 6 whole digits
  largest_normalized <- normalize_budget(max_budget, max_budget)
  expect_true(nchar(format(trunc(abs(largest_normalized)), scientific = FALSE, trim = TRUE)) <= 6)
})

test_that("normalize_budget returns original value when max_budget is less than 1M", {
  # Test with a small max_budget value
  max_budget <- 999999
  
  # Various budget values to test
  budget1 <- 500000
  budget2 <- 750000
  budget3 <- 100
  
  # Check that each value is returned unchanged
  expect_equal(normalize_budget(budget1, max_budget), budget1)
  expect_equal(normalize_budget(budget2, max_budget), budget2)
  expect_equal(normalize_budget(budget3, max_budget), budget3)
  
  # Edge case: exactly at the threshold
  threshold_max_budget <- 1000000
  expect_equal(normalize_budget(500000, threshold_max_budget), 500000 / 10)
})
