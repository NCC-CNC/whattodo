#' @include internal.R
NULL

#' Prioritization without budget
#'
#' Generate a prioritization without a budget.
#'
#' @param site_ids `character` site identifiers.
#'
#' @param feature_ids `character` feature identifiers.
#'
#' @param action_ids `character` action identifiers.
#'
#' @param pu_data `dat.frame` containing planning unit data.
#'
#' @param zone_data `dat.frame` containing zone data.
#'
#' @param goal_data `dat.frame` containing goal data.
#'
#' @param weight_data `dat.frame` containing weight data.
#'
#' @param locked_data `dat.frame` containing locked out data.
#'
#' @param budget `numeric` budget.
#'
#' @param parameters `list` object with configuration parameters.
#'
#' @param gap `numeric` optimality gap. Defaults to 0.
#'
#' @param verbose `logical` display information during optimization?
#'  Defaults to `TRUE`.
#'
#' @param time_limit `integer` Maximum amount of time (seconds) to spend
#'  during optimization.
#'  Defaults to the maximum integer value.
#'
#' @return `list` object containing results of prioritization.
#'
#' @export
prioritization_with_budget <- function(site_ids,
                                       feature_ids,
                                       action_ids,
                                       pu_data,
                                       zone_data,
                                       goal_data,
                                       weight_data,
                                       locked_data,
                                       budget,
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
    inherits(zone_data, "ZonesCharacter"),
    inherits(goal_data, "data.frame"),
    inherits(weight_data, "data.frame"),
    inherits(locked_data, "data.frame"),
    assertthat::is.number(budget),
    isTRUE(budget >= 0),
    assertthat::is.number(gap),
    isTRUE(gap >= 0),
    is.list(parameters),
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose),
    assertthat::is.count(time_limit),
    assertthat::noNA(time_limit)
  )

  # process cost names
  cost_names <- glue::glue(
    parameters$site_data_sheet$action_cost_header,
    action_ids = action_ids
  )

  # generate prioritization
  prb <-
    prioritizr::problem(pu_data, zone_data, cost_names) %>%
    prioritizr::add_min_shortfall_objective(budget = max(budget, 1e-5)) %>%
    prioritizr::add_feature_weights(matrix(weight_data[[2]], ncol = 1)) %>%
    prioritizr::add_manual_targets(goal_data) %>%
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
    zone_data = zone_data,
    goal_data = goal_data,
    locked_data = locked_data,
    solution_data = sol,
    budget = budget,
    parameters = parameters
  )

  # return results
  out
}
