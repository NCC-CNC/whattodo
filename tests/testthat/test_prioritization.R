context("prioritization")

test_that("prioritization_with_budget (feasible)", {
  # data
  set.seed(500)
  parameters <- app_parameters()
  d <- simulate_data(
    n_sites = 5, n_features = 3, n_actions = 4, parameters = parameters
  )
  d$feature_data[[3]] <- c(100, rep(0, nrow(d$feature_data) - 1))

  # prioritization
  r1 <- prioritization_with_budget(
    d$site_names, d$feature_names, d$action_names,
    d$site_data, d$site_status_data, d$feature_data, d$action_expectation_data,
    budget = 10, weights = FALSE, gap = 0, parameters
  )
  r2 <- prioritization_with_budget(
    d$site_names, d$feature_names, d$action_names,
    d$site_data, d$site_status_data, d$feature_data, d$action_expectation_data,
    budget = 10, weights = TRUE, gap = 0, parameters
  )

  # tests
  expect_is(r1, "list")
  expect_is(r1$summary_results, "data.frame")
  expect_is(r1$site_results, "data.frame")
  expect_is(r1$feature_results, "data.frame")
  expect_is(r2, "list")
  expect_is(r2$summary_results, "data.frame")
  expect_is(r2$site_results, "data.frame")
  expect_is(r2$feature_results, "data.frame")
  expect_false(identical(r1$site_results[[2]], r2$site_results[[2]]))
})

test_that("prioritization_with_budget (infeasible)", {
  # data
  set.seed(500)
  parameters <- app_parameters()
  d <- simulate_data(
    n_sites = 5, n_features = 3, n_actions = 4, parameters = parameters
  )
  d$site_status_data[1, seq(2, 5)] <- 0

  # prioritization
  r <- prioritization_with_budget(
    d$site_names, d$feature_names, d$action_names,
    d$site_data, d$site_status_data, d$feature_data, d$action_expectation_data,
    budget = 10, weights = FALSE, gap = 0, parameters
  )

  # tests
  expect_is(r, "list")
  expect_is(r$summary_results, "data.frame")
  expect_gte(nrow(r$summary_results), 1)
  expect_is(r$site_results, "data.frame")
  expect_is(r$feature_results, "data.frame")
})

test_that("prioritization_without_budget (feasible)", {
  # data
  parameters <- app_parameters()
  d <- simulate_data(
    n_sites = 5, n_features = 3, n_actions = 4, parameters = parameters
  )

  # prioritization
  r <- prioritization_without_budget(
    d$site_names, d$feature_names, d$action_names,
    d$site_data, d$site_status_data, d$feature_data, d$action_expectation_data,
    gap = 0, parameters
  )

  # tests
  expect_is(r, "list")
  expect_is(r$summary_results, "data.frame")
  expect_is(r$site_results, "data.frame")
  expect_is(r$feature_results, "data.frame")
})

test_that("prioritization_without_budget (infeasible)", {
  # data
  parameters <- app_parameters()
  d <- simulate_data(
    n_sites = 5, n_features = 3, n_actions = 4, parameters = parameters
  )
  d$feature_data[[2]] <- 10000

  # prioritization
  r <- prioritization_without_budget(
    d$site_names, d$feature_names, d$action_names,
    d$site_data, d$site_status_data, d$feature_data, d$action_expectation_data,
    gap = 0, parameters
  )

  # tests
  expect_is(r, "list")
  expect_is(r$summary_results, "data.frame")
  expect_gte(nrow(r$summary_results), 1)
  expect_is(r$site_results, "data.frame")
  expect_is(r$feature_results, "data.frame")
})
