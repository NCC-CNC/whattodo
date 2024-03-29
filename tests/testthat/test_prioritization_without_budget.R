context("prioritization_without_budget")

test_that("feasible problem", {
  # load data
  parameters <- whatdataio::read_data_configuration()
  d <- whatdataio::simulate_project_data(5, 3, 2, parameters)
  d$feature_data[[2]] <- 0.05 # manually force feasible goals
  p <- new_project(
    site_ids = d$site_ids,
    site_descriptions = d$site_descriptions,
    feature_ids = d$feature_ids,
    feature_descriptions = d$feature_descriptions,
    action_ids = d$action_ids,
    action_descriptions = d$action_descriptions,
    site_data = d$site_data,
    feature_data = d$feature_data,
    feasibility_data = d$feasibility_data,
    consequence_data = d$consequence_data,
    parameters = parameters,
    site_geometry = NULL
  )
  # create object
  x <- prioritization_without_budget(
    site_ids = p$site_ids,
    feature_ids = p$feature_ids,
    action_ids = p$action_ids,
    pu_data = p$get_pu_data(),
    status_data = p$get_current_status_data(),
    zone_data = p$get_zone_data(),
    goal_data = p$get_goal_data(),
    locked_data = p$get_locked_data(),
    gap = 0,
    parameters = parameters,
    verbose = FALSE
  )
  # tests
  expect_is(x, "list")
  expect_is(x$summary_results, "data.frame")
  expect_is(x$feature_results, "data.frame")
  expect_is(x$site_results, "data.frame")
})
