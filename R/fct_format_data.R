#' @include internal.R
NULL

#' Format planning unit data
#'
#' @param site_names `character` names of sites.
#'
#' @param feature_names `character` names of features.
#'
#' @param action_names `character` names of actions.
#'
#' @param site_data `data.frame` with site data.
#'
#' @param action_expectation_data `list` of `data.frame` objects with
#'  the expected amount of each feature given each action.
#'
#' @param parameters `list` object with configuration parameters.
#'
#' @return `data.frame` with planning unit data for prioritizations.
#'
#' @export
format_pu_data <- function(
  site_names, feature_names, action_names,
  site_data, action_expectation_data, parameters) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(site_names),
    is.character(feature_names),
    is.character(action_names),
    inherits(site_data, "data.frame"),
    inherits(action_expectation_data, "list"),
    is.list(parameters))
  # extract cost
  cost_names <- as.character(glue::glue(
      parameters$site_data_sheet$action_cost_header,
      action_names = action_names))
  cost_data <- site_data[, cost_names, drop = FALSE]
  names(cost_data) <- paste0("cost_", action_names)

  # extract action expectation data
  feat_col_names <- as.character(glue::glue(
    parameters$action_expectation_sheet$action_expectation_header,
    feature_names = feature_names))
  expectation_data <- lapply(seq_along(action_expectation_data), function(i) {
    out <- action_expectation_data[[i]]
    out <- out[, feat_col_names, drop = FALSE]
    names(out) <- paste0(action_names[i], "_", feature_names)
    out
  })

  # return result
  do.call(
    dplyr::bind_cols,
    append(list(
      tibble::tibble(site = site_names), cost_data),
      expectation_data))
}

#' Format zone data
#'
#' @inheritParams format_pu_data
#'
#' @return `data.frame` with zone data for prioritizations.
#'
#' @export
format_zone_data <- function(
  site_names, feature_names, action_names, parameters) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(site_names),
    is.character(feature_names),
    is.character(action_names),
    is.list(parameters))
  # create zone name column
  args <- lapply(action_names, function(x) paste0(x, "_", feature_names))
  # append feature and zone names to object
  args <- append(args,
    list(zone_names = action_names, feature_names = feature_names))
  # return zones object
  do.call(prioritizr::zones, args)
}

#' Format target data
#'
#' @inheritParams format_pu_data
#'
#' @param feature_data `data.frame` with feature data.
#'
#' @return `data.frame` with target data for prioritizations.
#'
#' @export
format_target_data <- function(
  site_names, feature_names, action_names, feature_data, parameters) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(site_names),
    is.character(feature_names),
    is.character(action_names),
    inherits(feature_data, "data.frame"),
    is.list(parameters))
  # prepare target data
  out <- tibble::tibble(
    feature = feature_names,
    zone = list(action_names)[rep(1, length(feature_names))],
    type = "absolute",
    sense = ">=",
    target = feature_data[[parameters$feature_data_sheet$target_header]])
  # return result
  out
}

#' Format weights data
#'
#' @inheritParams format_target_data
#'
#' @return `data.frame` with weights data for prioritizations.
#'
#' @export
format_weights_data <- function(
  site_names, feature_names, action_names, feature_data, parameters) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(site_names),
    is.character(feature_names),
    is.character(action_names),
    inherits(feature_data, "data.frame"),
    is.list(parameters))
  # prepare weights data
  out <- tibble::tibble(
    feature = feature_names,
    weight = feature_data[[parameters$feature_data_sheet$weight_header]])
  # return result
  out
}

#' Format locked data
#'
#' @param site_status_data `data.frame` with site status data.
#' @inheritParams format_pu_data
#'
#' @return `data.frame` with locked data for prioritizations.
#'
#' @export
format_locked_data <- function(
  site_names, feature_names, action_names, site_status_data, parameters) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(site_names),
    is.character(feature_names),
    is.character(action_names),
    inherits(site_status_data, "data.frame"),
    is.list(parameters))
  # define variables to avoid CRAN checks throwing ERRORS for lazy evaluation
  action <- NULL
  status <- NULL
  site <- NULL
  # prepare data
  d <- site_status_data
  names(d) <- c("site", action_names)
  d <- tidyr::gather(d, action, status, -site)
  d <- d[d$status <= 0.5, , drop = FALSE]
  d$id <- match(d$site, site_names)
  assertthat::assert_that(assertthat::noNA(d$id))
  # return result
  tibble::tibble(
    pu = d$id,
    zone = d$action,
    status = 0)
}

#' Format results data
#'
#' Format the results from a prioritization for the application.
#'
#' @inheritParams format_pu_data
#'
#' @param pu_data `data.frame` containing planning unit data used to
#'   create prioritization.
#'
#' @param zone_data `data.frame` containing zone data used to
#'   create prioritization.
#'
#' @param target_data `data.frame` containing target data used to
#'   create prioritization.
#'
#' @param solution `data.frame` containing solution data from
#'   running the prioritization.
#'
#' @return `list` object contain `data.frame` objects summarizing the results.
#'
#' @export
format_results_data <- function(
  site_names, feature_names, action_names,
  pu_data, zone_data, target_data, solution, parameters) {
  # assert that arguments are valid
  assertthat::assert_that(
    is.character(site_names),
    is.character(feature_names),
    is.character(action_names),
    inherits(pu_data, "data.frame"),
    inherits(zone_data, "ZonesCharacter"),
    inherits(target_data, "data.frame"),
    inherits(solution, "data.frame"))

  # declare parameters
  sol_names <- paste0("solution_1_", action_names)
  assertthat::assert_that(all(assertthat::has_name(solution, sol_names)))

  # summary results
  ## calculate total cost
  summary_cost_name <- parameters$summary_results_sheet$cost_statistic_name
  summary_cost_value <-
    sum(vapply(seq_along(action_names), FUN.VALUE = numeric(1), function(i) {
      sum(solution[[sol_names[[i]]]] *
          pu_data[[paste0("cost_", action_names[i])]])
    }))

  ## calculate number of sites allocated to each action
  summary_n_action_names <- as.character(glue::glue(
    parameters$summary_results_sheet$n_action_statistic_name,
    action_names = action_names))
  summary_n_action_value <-
    vapply(sol_names, FUN.VALUE = numeric(1), function(x) {
      sum(solution[[x]])
  })
  ## prepare summary
  summary_results <- tibble::tibble(
    name = c(summary_cost_name, summary_n_action_names),
    value = c(summary_cost_value, summary_n_action_value))
  names(summary_results) <- c(
    parameters$summary_results_sheet$name_header,
    parameters$summary_results_sheet$value_header)

  # feature representation results
  feature_results <-
    vapply(
      seq_along(sol_names),
      FUN.VALUE = numeric(length(feature_names)),
      function(i) {
        # extract rij values for action
        rij <- as.matrix(pu_data[, zone_data[[i]], drop = FALSE])
        # extract solution values for action
        s <- matrix(solution[[sol_names[[i]]]], nrow = length(site_names),
                    ncol = length(feature_names))
        # compute amount held in zone
        colSums(rij * s)
    })
  if (!is.matrix(feature_results))
    feature_results <- matrix(feature_results, nrow = length(feature_names))
  feature_totals <- rowSums(feature_results)
  feature_results <- tibble::as_tibble(as.data.frame(feature_results))

  names(feature_results) <- action_names
  feature_results <- tibble::tibble(
    name = feature_names, feature_results, total = feature_totals)
  names(feature_results) <- c(
    parameters$feature_results_sheet$name_header,
    sapply(action_names, function(x) {
      as.character(
        glue::glue(parameters$feature_results_sheet$action_amount_header,
          action_names = x))
    }),
    parameters$feature_results_sheet$total_amount_header)

  # solution status results
  site_results <- solution[, sol_names, drop = FALSE]
  site_results <- as.matrix(site_results)
  site_results <- prioritizr::category_vector(site_results)
  site_results <- action_names[site_results]
  site_results <- tibble::tibble(name = site_names, action = site_results)
  names(site_results) <- c(
    parameters$site_results_sheet$name_header,
    parameters$site_results_sheet$action_header)

  # return result
  list(summary_results = summary_results,
       feature_results = feature_results,
       site_results = site_results)
}

#' Format error data
#'
#' Format results for a prioritization problem that threw an error.
#'
#' @param problem `ConservationProblem` object.
#'
#' @inheritParams format_results_data
#' @inheritParams format_error_data
#'
#' @inherit format_results_data return
#'
#' @export
format_error_data <- function(
  site_names, feature_names, action_names, problem, parameters) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(site_names),
    is.character(feature_names),
    is.character(feature_names),
    inherits(problem, "ConservationProblem"),
    is.list(parameters))

  # make summary table
  if (requireNamespace("gurobi", quietly = TRUE)) {
    ## prepare labels
    status_labels <- c(sapply(action_names, function(x) {
      as.character(
        glue::glue(parameters$error_sheets$status_prefix,
          action_names = x, site_names = site_names))
    }))
    target_labels <-
      as.character(glue::glue(
        parameters$error_sheets$target_prefix,
        feature_names = feature_names))
    ## prepare optimization problem data
    op <- prioritizr::compile(problem)
    model <- list(
      modelsense = op$modelsense(),
      obj = op$obj(), A = op$A(),
      vtype = op$vtype(), rhs = op$rhs(), sense = op$sense(),
      lb = op$lb(), ub = op$ub())
    ## compute IIS
    withr::with_locale(
      c(LC_CTYPE = "C"),
      {iis <- gurobi::gurobi_iis(model)})
    ## extract ISS
    lb_iis <- iis$lb[seq_along(status_labels)]
    ub_iis <- iis$ub[seq_along(status_labels)]
    rhs_iis <- iis$Arows[seq_along(target_labels)]
    budget_iis <- iis$Arows[length(target_labels) + 1]
    ## prepare text
    lower_text <- paste(
      status_labels[lb_iis],
      rep(parameters$error_sheets$lower_bound_message, sum(lb_iis)))
    upper_text <- paste(
      status_labels[ub_iis],
      rep(parameters$error_sheets$upper_bound_message, sum(ub_iis)))
    target_text <- paste(
      target_labels[rhs_iis],
      rep(parameters$error_sheets$target_message, sum(rhs_iis)))
    budget_text <- paste(
      parameters$error_sheets$budget_prefix[budget_iis],
      rep(parameters$error_sheets$budget_message, sum(budget_iis)))
    ## prepare table
    summary_data <- tibble::tibble(
      text = c(lower_text, upper_text, target_text, budget_text))
    names(summary_data) <- parameters$error_sheets$iis_header
  } else {
    ## prepare table
    summary_data <- tibble::tibble(name = parameters$error_sheets$main_message)
    names(summary_data) <- parameters$error_sheets$name_header
  }

  # create default error data
  error_data <- tibble::tibble(
    name = parameters$error_sheet$sub_message)
  names(error_data) <- parameters$error_sheets$name_header

  # return solution
  list(summary_results = summary_data,
       feature_results = error_data,
       site_results = error_data)
}
