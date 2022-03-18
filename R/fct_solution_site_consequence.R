#' @include internal.R
NULL

#' Calculate site consequence data for solution
#'
#' This function calculates the consequences for each site
#' within a solution. Specifically, it shows how each site contributes
#' to the overall amount of each feature given the solution.
#'
#' @param feature_ids `character` vector of identifiers for each feature.
#'
#' @param action_ids `character` vector of identifiers for each action.
#'
#' @param pu_data `data.frame` containing planning unit data.
#'
#' @param solution_data `data.frame` object containing the solution.
#'
#' @return A `matrix` object containing the expected
#'   amount of each feature within each site.
#'   Each row is a different planning unit, each column is a different feature.
#'
#' @export
solution_site_consequence <- function(feature_ids, action_ids, pu_data,
                                      solution_data) {
  # assert arguments are valid
  assertthat::assert_that(
    ## action_ids
    is.character(action_ids),
    assertthat::noNA(action_ids),
    identical(anyDuplicated(action_ids), 0L),
    length(action_ids) >= 1,
    ## feature_ids
    is.character(feature_ids),
    assertthat::noNA(feature_ids),
    identical(anyDuplicated(feature_ids), 0L),
    length(feature_ids) >= 1,
    ## pu_data
    inherits(pu_data, "data.frame"),
    nrow(pu_data) >= 1,
    ## solution_data
    inherits(solution_data, "data.frame"),
    nrow(solution_data) >= 1,
    all(assertthat::has_name(solution_data, paste0("solution_1_", action_ids)))
  )
  action_combs <- apply(
    as.matrix(expand.grid(action_ids, feature_ids)), 1, paste, collapse = "_")
  assertthat::assert_that(all(assertthat::has_name(pu_data, action_combs)))

  # calculate consequences
  out <- matrix(0, nrow = nrow(pu_data), ncol = length(feature_ids))
  colnames(out) <- feature_ids
  rownames(out) <- pu_data$site
  for (j in feature_ids) {
    for (i in action_ids) {
      n <- paste0(i, "_", j)
      v <- solution_data[[paste0("solution_1_", i)]] * pu_data[[n]]
      out[, j] <- out[, j] + v
    }
  }

  # return result
  out
}
