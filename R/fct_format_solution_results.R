#' @include internal.R
NULL

#' Format solution results
#'
#' Format the results from a prioritization for the application.
#'
#' @inheritParams prioritization_with_budget
#'
#' @param solution_data `data.frame` containing solution data from
#'   running the prioritization.
#'
#' @return `list` object containing `data.frame` objects with results.
#'
#' @export
format_solution_results <- function(site_ids,
                                    feature_ids,
                                    action_ids,
                                    pu_data,
                                    zone_data,
                                    goal_data,
                                    locked_data,
                                    solution_data,
                                    budget,
                                    parameters) {

  # assert that arguments are valid
  assertthat::assert_that(
    is.character(site_ids),
    is.character(feature_ids),
    is.character(action_ids),
    inherits(pu_data, "data.frame"),
    inherits(zone_data, "ZonesCharacter"),
    inherits(goal_data, "data.frame"),
    inherits(locked_data, "data.frame"),
    inherits(solution_data, "data.frame"),
    is.list(parameters)
  )

  # declare parameters
  sol_names <- paste0("solution_1_", action_ids)
  assertthat::assert_that(all(assertthat::has_name(solution_data, sol_names)))

  # summary results
  ## calculate total cost
  summary_cost_name <- parameters$summary_results_sheet$cost_statistic_name
  summary_cost_value <-
    sum(vapply(seq_along(action_ids), FUN.VALUE = numeric(1), function(i) {
      sum(
        solution_data[[sol_names[[i]]]] *
        pu_data[[
          glue::glue(
            parameters$site_data_sheet$action_cost_header,
            action_ids = action_ids[i]
          )
        ]]
      )
    }))

  ## calculate number of sites allocated to each action
  summary_n_action_ids <- as.character(glue::glue(
    parameters$summary_results_sheet$n_action_statistic_name,
    action_ids = action_ids
  ))
  summary_n_action_value <-
    vapply(sol_names, USE.NAMES = FALSE, FUN.VALUE = numeric(1), function(x) {
      sum(solution_data[[x]])
    })
  ## prepare summary
  summary_results <- tibble::tibble(
    name = c(summary_cost_name, summary_n_action_ids),
    value = c(summary_cost_value, summary_n_action_value)
  )
  names(summary_results) <- c(
    parameters$summary_results_sheet$name_header,
    parameters$summary_results_sheet$value_header
  )

  # feature representation results
  feature_results <-
    vapply(
      seq_along(sol_names),
      FUN.VALUE = numeric(length(feature_ids)),
      USE.NAMES = FALSE,
      function(i) {
        # extract rij values for action
        rij <- as.matrix(pu_data[, zone_data[[i]], drop = FALSE])
        # extract solution_data values for action
        s <- matrix(solution_data[[sol_names[[i]]]],
          nrow = length(site_ids),
          ncol = length(feature_ids)
        )
        # compute amount held in zone
        colSums(rij * s)
      }
    )
  if (!is.matrix(feature_results)) {
    feature_results <- matrix(feature_results, nrow = length(feature_ids))
  }
  feature_totals <- unname(rowSums(feature_results))
  feature_results <- tibble::as_tibble(as.data.frame(feature_results))

  names(feature_results) <- action_ids
  feature_results <- tibble::tibble(
    name = feature_ids, feature_results, total = feature_totals
  )
  names(feature_results) <- c(
    parameters$feature_results_sheet$name_header,
    sapply(action_ids, function(x) {
      as.character(
        glue::glue(parameters$feature_results_sheet$action_amount_header,
          action_ids = x
        )
      )
    }),
    parameters$feature_results_sheet$total_amount_header
  )

  # solution_data status results
  site_results <- solution_data[, sol_names, drop = FALSE]
  site_results <- as.matrix(site_results)
  site_results <- prioritizr::category_vector(site_results)
  site_results <- action_ids[site_results]
  site_results <- tibble::tibble(name = site_ids, action = site_results)
  names(site_results) <- c(
    parameters$site_results_sheet$name_header,
    parameters$site_results_sheet$action_header
  )

  # return result
  list(
    summary_results = summary_results,
    feature_results = feature_results,
    site_results = site_results,
    budget = budget
  )
}
