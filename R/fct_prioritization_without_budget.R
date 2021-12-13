#' @include internal.R
NULL

#' Prioritization without budget
#'
#' Generate a prioritization without a budget.
#'
#' @inheritParams prioritization_with_budget
#'
#' @inherit prioritization_with_budget return
#'
#' @export
prioritization_without_budget <- function(site_ids,
                                          feature_ids,
                                          action_ids,
                                          pu_data,
                                          zone_data,
                                          goal_data,
                                          locked_data,
                                          parameters,
                                          gap = 0) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(site_ids),
    assertthat::noNA(site_ids),
    is.character(feature_ids),
    assertthat::noNA(feature_ids),
    is.character(action_ids),
    assertthat::noNA(action_ids),
    inherits(pu_data, "data.frame"),
    inherits(zone_data, "ZonesCharacter"),
    inherits(goal_data, "data.frame"),
    inherits(locked_data, "data.frame"),
    assertthat::is.number(gap),
    isTRUE(gap >= 0),
    is.list(parameters)
  )

  # process cost names
  cost_names <- glue::glue(
    parameters$site_data_sheet$action_cost_header,
    action_ids = action_ids
  )

  # generate prioritization
  prb <-
    prioritizr::problem(pu_data, zone_data, cost_names) %>%
    prioritizr::add_min_set_objective() %>%
    prioritizr::add_manual_targets(goal_data) %>%
    prioritizr::add_mandatory_allocation_constraints() %>%
    prioritizr::add_binary_decisions() %>%
    prioritizr::add_default_solver(gap = gap, verbose = FALSE)
  if (nrow(locked_data) > 0) {
    prb <-
      prb %>%
      prioritizr::add_manual_locked_constraints(locked_data)
  }
  sol <- try(prioritizr::solve(prb), silent = TRUE)

  # summarize results
  if (inherits(sol, "try-error")) {
    out <- format_solution_error(parameters = parameters)
    out$solved <- FALSE
  } else {
    out <- format_solution_results(
      site_ids = site_ids,
      feature_ids = feature_ids,
      action_ids =  action_ids,
      pu_data = pu_data,
      zone_data = zone_data,
      goal_data = goal_data,
      locked_data = locked_data,
      solution_data = sol,
      budget = NA,
      parameters = parameters
    )
    out$solved <- TRUE
  }

  # return results
  out
}
