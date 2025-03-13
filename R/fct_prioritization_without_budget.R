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
    is.list(pu_data),
    inherits(pu_data$raw_data, "data.frame"),
    inherits(pu_data$norm_data, "data.frame") | is.null(pu_data$norm_data),
    inherits(pu_data$norm_data, "data.frame"),
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

  # set problem data to use raw or normalized costs to solve
  if (is.null(pu_data$norm_data)) {
    prb_pu_data <- pu_data$raw_data
  } else {
    prb_pu_data <- pu_data$norm_data
  }
  
  # generate prioritization
  prb <-
    prioritizr::problem(prb_pu_data, zone_data, as.character(cost_names)) %>%
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
  
  sol <- prioritizr::solve.ConservationProblem(prb)

  # summarize results
  out <- format_solution_results(
    site_ids = site_ids,
    feature_ids = feature_ids,
    action_ids =  action_ids,
    pu_data = pu_data$raw_data, # evaluate solution based on raw data
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
