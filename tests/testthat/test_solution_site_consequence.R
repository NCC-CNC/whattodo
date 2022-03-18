context("solution_site_consequence")

test_that("expected result", {
  # load data
  parameters <- whatdataio::read_data_configuration()
  d <- whatdataio::simulate_project_data(5, 3, 2, parameters)
  # create object
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
  # prepare solution
  sol <- tibble::as_tibble(matrix(
    0, nrow = length(p$site_ids), ncol = length(p$action_ids),
    dimnames = list(NULL, paste0("solution_1_", p$action_ids))
  ))
  for (i in seq_len(nrow(d$site_data))) {
    sol[[match(d$site_data[["Current status"]][[i]], p$action_ids)]][[i]] <- 1
  }
  pu_data <- p$get_pu_data()
  # create object
  x <- solution_site_consequence(
    p$feature_ids, p$action_ids, pu_data, sol)
  # tests
  expect_is(x, "matrix")
  expect_equal(nrow(x), length(p$site_ids))
  expect_equal(ncol(x), length(p$feature_ids))
  expect_equal(rownames(x), p$site_ids)
  expect_equal(colnames(x), p$feature_ids)
  expect_equal(
    x[, "feature 1"],
    setNames(
      (pu_data[["action 1_feature 1"]] * sol[["solution_1_action 1"]]) +
        (pu_data[["action 2_feature 1"]] * sol[["solution_1_action 2"]]),
      p$site_ids
    )
  )
  expect_equal(
    x[, "feature 2"],
    setNames(
      (pu_data[["action 1_feature 2"]] * sol[["solution_1_action 1"]]) +
        (pu_data[["action 2_feature 2"]] * sol[["solution_1_action 2"]]),
      p$site_ids
    )
  )
  expect_equal(
    x[, "feature 3"],
    setNames(
      (pu_data[["action 1_feature 3"]] * sol[["solution_1_action 1"]]) +
        (pu_data[["action 2_feature 3"]] * sol[["solution_1_action 2"]]),
      p$site_ids
    )
  )
})
