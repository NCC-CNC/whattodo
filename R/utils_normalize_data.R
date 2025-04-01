#' @include internal.R
NULL

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
#' Scales cost-related columns
#'
#' @param x A `tibble` containing raw planning unit data.
#' @param max_budget A `numeric` greatest cost value for each site summed.
#' @param col_starts_with A `character` string indicating the prefix for 
#' cost-related columns. Default is `"Cost"`.
#'
#' @details
#' If the `max_budget` is **1,000,000 or greater**, the function determines the number 
#' of whole number digits and calculates a divisor to scale it down to six digits.  
#' 
#' @return A `tibble` with normalized cost columns and the computed divisor, 
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
#' # Compute max budget
#' max_budget <- max(df %>% select(starts_with("Cost")), na.rm = TRUE)
#'
#' # Normalize cost columns
#' normalize_cost_columns(df, max_budget)
#'
#' @export
normalize_cost_columns <- function(x, max_budget, col_starts_with = "Cost") {
  
  # check if normalization is needed
  if (max_budget >= 1000000) {
    # determine the number of whole number digits in the max_budget value
    num_digits <- num_whole_digits(max_budget)
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
#' Scales a single numeric budget value.
#'
#' @param budget A `numeric` value representing the budget.
#' @param max_budget A `numeric` greatest cost value for each site summed.
#'
#' @details
#' If the `max_budget` is **1,000,000 or greater**, the function determines the number 
#' of whole number digits and calculates a divisor to scale it down to six digits.  
#' If no scaling is needed, the original budget is returned unchanged.
#'
#' @return A `numeric` value representing the normalized budget.
#'
#' @examples
#' # Example usage with different max_budget values
#' normalize_budget(5000000, max_budget = 10000000)  # Normalized based on max_budget
#' 
#' @export
normalize_budget <- function(budget, max_budget) {
  # Check if normalization is needed
  if (max_budget >= 1000000) {
    # Determine the number of whole number digits in max_budget
    num_digits <- num_whole_digits(max_budget)
    # Calculate the divisor
    divisor <- 10^(num_digits - 6)
    # Return normalized budget
    return(budget / divisor)
  } else {
    # Return budget if no scaling is needed
    return(budget)
  }
}

