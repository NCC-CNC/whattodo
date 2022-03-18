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
                                          status_data,
                                          zone_data,
                                          goal_data,
                                          locked_data,
                                          parameters,
                                          gap = 0,
                                          verbose = TRUE,
                                          time_limit = .Machine$integer.max) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(site_ids),
    assertthat::noNA(site_ids),
    is.character(feature_ids),
    assertthat::noNA(feature_ids),
    is.character(action_ids),
    assertthat::noNA(action_ids),
    inherits(pu_data, "data.frame"),
    inherits(status_data, "data.frame"),
    inherits(zone_data, "ZonesCharacter"),
    inherits(goal_data, "data.frame"),
    inherits(locked_data, "data.frame"),
    assertthat::is.number(gap),
    isTRUE(gap >= 0),
    is.list(parameters),
    assertthat::is.count(time_limit),
    assertthat::noNA(time_limit)
  )

  # process cost names
  cost_names <- glue::glue(
    parameters$site_data_sheet$action_cost_header,
    action_ids = action_ids
  )

  # prepare target data
  target_data <-
    goal_data[, c("feature", "zone", "type", "sense", "target"), drop = FALSE]

  # generate prioritization
  prb <-
    prioritizr::problem(pu_data, zone_data, cost_names) %>%
    prioritizr::add_min_set_objective() %>%
    prioritizr::add_manual_targets(target_data) %>%
    prioritizr::add_mandatory_allocation_constraints() %>%
    prioritizr::add_binary_decisions() %>%
    prioritizr::add_default_solver(
      gap = gap, verbose = verbose, time_limit = time_limit
    )
  if (nrow(locked_data) > 0) {
    prb <-
      prb %>%
      prioritizr::add_manual_locked_constraints(locked_data)
  }
  sol <- prioritizr::solve(prb)

  # summarize results
  out <- format_solution_results(
    site_ids = site_ids,
    feature_ids = feature_ids,
    action_ids =  action_ids,
    pu_data = pu_data,
    status_data = status_data,
    zone_data = zone_data,
    goal_data = goal_data,
    locked_data = locked_data,
    solution_data = sol,
    budget = NA,
    parameters = parameters
  )

  # return results
  out
}
