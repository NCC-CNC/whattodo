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
#' @param gap `numeric` optimality gap.
#'
#' @param parameters `list` object with configuration parameters.
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
                                       gap = 0,
                                       parameters) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(pu_data, "data.frame"),
    inherits(zone_data, "data.frame"),
    inherits(goal_data, "data.frame"),
    inherits(weight_data, "data.frame"),
    inherits(locked_data, "data.frame"),
    assertthat::is.number(budget),
    isTRUE(budget >= 0),
    assertthat::is.flag(weights),
    assertthat::noNA(weights),
    assertthat::is.number(gap),
    isTRUE(gap >= 0),
    is.list(parameters)
  )

  # generate prioritization
  prb <-
    prioritizr::problem(pu_data, zone_data, paste0("cost_", action_ids)) %>%
    prioritizr::add_min_shortfall_objective(budget = max(budget, 1e-5)) %>%
    prioritizr::add_feature_weights(matrix(weights_data[[2]], ncol = 1)) %>%
    prioritizr::add_manual_targets(target_data) %>%
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
      target_data = target_data,
      solution_data = sol,
      budget = budget,
      parameters = parameters
    )
    out$solved <- TRUE
  }

  # return results
  out
}
