#' @include internal.R
NULL

#' Get Maximum Cost Value
#'
#' Extract the highest cost value among all cost-related columns in a tibble.
#' This is useful to determine whether normalization is required.
#'
#' @param x A `tibble` containing raw planning unit data (e.g., `curr_pu_data`).
#' @param col_starts_with A `character` string indicating the prefix for cost 
#'   columns. Default is `"Cost"`.
#'
#' @details
#' The function scans all columns that start with `col_starts_with`, selects 
#' the highest cost value, and returns it as a numeric value.
#'
#' @return A `numeric` value representing the maximum cost.
#'
#' @examples
#' library(dplyr)
#' 
#' # Example tibble
#' df <- tibble(
#'   ID = 1:3,
#'   CostA = c(1234567, 987654321, 54321),
#'   CostB = c(10000000, 2500, 99999999),
#'   Value = c(10, 20, 30)
#' )
#' 
#' # Get max cost
#' max_cost(df)
#' 
#' # Using a different column prefix
#' df_renamed <- rename(df, ExpenseA = CostA, ExpenseB = CostB)
#' max_cost(df_renamed, col_starts_with = "Expense")
#' 
#' @export
max_cost <- function(x, col_starts_with = "Cost") {
  # Select columns that start with the specified prefix
  cost_cols <- x %>%
    dplyr::select(dplyr::starts_with(col_starts_with))
  
  # Check if any matching columns were found
  if (ncol(cost_cols) == 0) {
    stop(paste0("No columns starting with '", col_starts_with, "' found in the input data."))
  }
  
  # Get the maximum value
  cost_cols %>%
    range(na.rm = TRUE) %>%
    .[2]
}

#' Count Whole Number Digits
#'
#' Determines the number of whole number digits in a numeric value.
#'
#' @param x A `numeric` value or vector.
#'
#' @details
#' Calculates the number of digits before the decimal point
#' in the absolute value of `x`. It truncates any decimal places and
#' ensures that scientific notation is not used when counting digits.
#'
#' @return An `integer` representing the number of whole number digits.
#'
#' @examples
#' num_whole_digits(12345)   # Returns 5
#' num_whole_digits(0.1234)  # Returns 1
#' num_whole_digits(1e6)     # Returns 7
#'
#' @export
num_whole_digits <- function(x) {
  nchar(format(trunc(abs(x)), scientific = FALSE, trim = TRUE))
}

#' Normalize Cost Columns
#'
#' Scales cost-related columns to ensure that the maximum value has at most six 
#' whole number digits.
#'
#' @param x A `tibble` containing raw planning unit data.
#' @param col_starts_with A `character` string indicating the prefix for 
#' cost-related columns. Default is `"Cost"`.
#'
#' @details
#' This function checks the highest cost value across all columns starting 
#' with `col_starts_with`. If the maximum cost is **1,000,000 or greater**, it 
#' determines the number of whole number digits and calculates a divisor to scale 
#' the values down to six digits. The function returns a modified 
#' tibble with the cost columns normalized. If no scaling is needed, 
#' it returns `NULL`.
#'
#' @return A `tibble` with normalized cost columns, 
#' or `NULL` if no scaling is required.
#'
#' @examples
#' library(dplyr)
#'
#' # Example tibble
#' df <- tibble(
#'   ID = 1:3,
#'   CostA = c(1e7, 500000, 2000000),  # Large values to test normalization
#'   CostB = c(2500, 999999, 12345678),
#'   Value = c(10, 20, 30)
#' )
#'
#' # Normalize cost columns
#' normalize_cost_columns(df)
#'
#' @export
normalize_cost_columns <- function(x, col_starts_with = "Cost") {
  
  # get the max cost value across all cost-related columns
  max_cost <- max_cost(x, col_starts_with = col_starts_with)
  # check if normalization is needed
  if (max_cost >= 1000000) {
    # determine the number of whole number digits in the max cost value
    num_digits <- num_whole_digits(max_cost)
    # calculate the divisor to scale values down to at most 6 whole digits
    divisor <- 10^(num_digits - 6)
    # apply the scaling transformation to all cost-related columns
    curr_pu_data_norm <- x %>%
      dplyr::mutate(
        dplyr::across(dplyr::starts_with(col_starts_with), ~ .x / divisor)
        )
    # return the normalized tibble
    return(curr_pu_data_norm)
  } else {
    # if no scaling is needed, return NULL
    return(NULL)
  }
}

#' Normalize Budget Value
#'
#' Scales a single numeric budget value to ensure it has at most six whole number digits.
#'
#' @param budget A `numeric` value representing the budget.
#'
#' @details
#' If the budget is **1,000,000 or greater**, the function determines the number 
#' of whole number digits and calculates a divisor to scale it down to six digits.  
#' If no scaling is needed, the original budget is returned unchanged.
#'
#' @return A `numeric` value representing the normalized budget.
#'
#' @examples
#' normalize_budget(5000000)   # Returns 500.000
#' normalize_budget(250000)    # Returns 250000 (unchanged)
#'
#' @export
normalize_budget <- function(budget) {
  # Check if normalization is needed
  if (budget >= 1000000) {
    # Determine the number of whole number digits
    num_digits <- num_whole_digits(budget)
    # Calculate the divisor
    divisor <- 10^(num_digits - 6)
    # Return normalized budget
    return(budget / divisor)
  } else {
    # Return NULL if no scaling is needed
    return(budget)
  }
}

