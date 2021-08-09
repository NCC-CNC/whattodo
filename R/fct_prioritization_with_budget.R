#' @include internal.R fct_format_data.R
NULL

#' Prioritization without budget
#'
#' Generate a prioritization without a budget.
#'
#' @inheritParams format_pu_data
#' @inheritParams format_locked_data
#' @inheritParams format_target_data
#'
#' @param budget `numeric` budget.
#'
#' @param weights `logical` use weights?
#'
#' @param gap `numeric` optimality gap.
#'
#' @return `list` object containing results of prioritization.
#'
#' @export
prioritization_with_budget <- function(
  site_names, feature_names, action_names,
  site_data, site_status_data, feature_data, action_expectation_data,
  budget, weights = TRUE, gap = 0, parameters) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(site_data, "data.frame"),
    inherits(site_status_data, "data.frame"),
    inherits(feature_data, "data.frame"),
    inherits(action_expectation_data, "list"),
    assertthat::is.number(budget),
    isTRUE(budget >= 0),
    assertthat::is.flag(weights),
    assertthat::noNA(weights),
    assertthat::is.number(gap),
    isTRUE(gap >= 0),
    is.list(parameters))

  # prepare data for prioritization
  pu_data <- format_pu_data(
    site_names, feature_names, action_names,
    site_data, action_expectation_data, parameters)
  zone_data <- format_zone_data(
    site_names, feature_names, action_names, parameters)
  target_data <- format_target_data(
    site_names, feature_names, action_names, feature_data, parameters)
  weights_data <- format_weights_data(
    site_names, feature_names, action_names, feature_data, parameters)
  locked_data <- format_locked_data(
      site_names, feature_names, action_names, site_status_data, parameters)

  # manually set weights to equal if weights == FALSE
  if (!isTRUE(weights)) {
    weights_data[[2]] <- 1
  }

  # generate prioritization
  prb <-
    prioritizr::problem(pu_data, zone_data, paste0("cost_", action_names)) %>%
    prioritizr::add_min_shortfall_objective(budget = max(budget, 1e-5)) %>%
    prioritizr::add_feature_weights(matrix(weights_data[[2]], ncol = 1)) %>%
    prioritizr::add_manual_targets(target_data) %>%
    prioritizr::add_mandatory_allocation_constraints() %>%
    prioritizr::add_binary_decisions() %>%
    prioritizr::add_cbc_solver(gap = gap, verbose = FALSE)
  if (nrow(locked_data) > 0)
    prb <-
      prb %>%
      prioritizr::add_manual_locked_constraints(locked_data)
  sol <- try(prioritizr::solve(prb), silent = TRUE)

  # summarize results
  if (inherits(sol, "try-error")) {
    out <- format_error_data(
      site_names, feature_names, action_names, prb, parameters)
    out$solved <- FALSE
  } else {
    out <- format_results_data(
      site_names, feature_names, action_names,
      pu_data, zone_data, target_data, sol, parameters)
    out$solved <- TRUE
  }

  # return results
  out
}
