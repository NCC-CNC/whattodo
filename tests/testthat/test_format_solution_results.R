context("format_solution_results")

test_that("without budget", {
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
  # generate object
  x <- format_solution_results(
    site_ids = p$site_ids,
    feature_ids = p$feature_ids,
    action_ids = p$action_ids,
    pu_data = p$get_pu_data(),
    status_data = p$get_current_status_data(),
    zone_data = p$get_zone_data(),
    goal_data = p$get_goal_data(),
    locked_data = p$get_locked_data(),
    solution_data = sol,
    budget = NA_real_,
    parameters = parameters
  )
  # tests
  ## summary results
  expect_true(assertthat::has_name(x, "summary_results"))
  expect_equal(ncol(x$summary_results), 2)
  expect_equal(nrow(x$summary_results), 3)
  expect_equal(
    x$summary_results[[1]],
    c(
      "Total cost",
      paste0("Number of sites allocated to “", p$action_ids, "”")
    )
  )
  expect_equal(
    x$summary_results[[2]],
    c(
      sum(
        sol[[1]] * p$get_action_costs(p$action_ids[[1]]),
        sol[[2]] * p$get_action_costs(p$action_ids[[2]])
      ),
      sum(sol[[1]]),
      sum(sol[[2]])
    )
  )
  ## feature results
  expect_true(assertthat::has_name(x, "feature_results"))
  expect_equal(x$feature_results[[1]], p$feature_ids)
  expect_equal(
    x$feature_results[[2]],
    paste0(
        p$get_goal_data()$goal, " (",
        p$get_goal_data()$goal * p$get_goal_data()$max, " units)"
    )
  )
  fh <- 
  expect_equal(
    x$feature_results[[3]],
    c(
      paste0(



      )
        p$get_goal_data()$goal, " (",
        , " units)"
    )
  )


    held = paste0(
      round(feature_absolute_held / goal_data$max, 2),
       "% (", feature_absolute_held, " units)"
    ),
    goal_met = dplyr::if_else(
      feature_absolute_held >= goal_data$goal * goal_data$max,  "Yes", "No"
    )


    c(
      sum(
        sol[[1]] * p$get_consequences_for_feature(
          p$action_ids[[1]],
          p$feature_ids[[1]]
        )
      ),
      sum(
        sol[[1]] * p$get_consequences_for_feature(
          p$action_ids[[1]],
          p$feature_ids[[2]]
        )
      ),
      sum(
        sol[[1]] * p$get_consequences_for_feature(
          p$action_ids[[1]],
          p$feature_ids[[3]]
        )
      )
    )
  )
  expect_equal(
    x$feature_results[[3]],
    c(
      sum(
        sol[[2]] * p$get_consequences_for_feature(
          p$action_ids[[2]],
          p$feature_ids[[1]]
        )
      ),
      sum(
        sol[[2]] * p$get_consequences_for_feature(
          p$action_ids[[2]],
          p$feature_ids[[2]]
        )
      ),
      sum(
        sol[[2]] * p$get_consequences_for_feature(
          p$action_ids[[2]],
          p$feature_ids[[3]]
        )
      )
    )
  )
  expect_equal(
    x$feature_results[[4]],
    x$feature_results[[2]] + x$feature_results[[3]]
  )
  ## site results
  expect_true(assertthat::has_name(x, "site_results"))
  expect_equal(ncol(x$site_results), 4)
  expect_equal(nrow(x$site_results), 5)
  expect_equal(
    x$site_results[[1]],
    p$site_ids
  )
  expect_equal(
    x$site_results[[2]],
    p$get_current_status_data()[[1]]
  )
  expect_equal(
    x$site_results[[3]],
    vapply(seq_len(nrow(sol)), FUN.VALUE = character(1), function(i) {
      p$action_ids[[which(c(as.matrix(sol[i, ])) > 0.5)]]
    })
  )
  expect_equal(
    x$site_results[[4]],
    vapply(seq_len(nrow(sol)), FUN.VALUE = numeric(1), function(i) {
      a <- which(c(as.matrix(sol[i, ])) > 0.5)
      p$get_action_costs(p$action_ids[[a]])[[i]]
    })
  )
  ## budget
  expect_true(assertthat::has_name(x, "budget"))
  expect_equal(x$budget, NA_real_)
})

test_that("with budget", {
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
  # generate object
  x <- format_solution_results(
    site_ids = p$site_ids,
    feature_ids = p$feature_ids,
    action_ids = p$action_ids,
    pu_data = p$get_pu_data(),
    status_data = p$get_current_status_data(),
    zone_data = p$get_zone_data(),
    goal_data = p$get_goal_data(),
    locked_data = p$get_locked_data(),
    solution_data = sol,
    budget = 5.23,
    parameters = parameters
  )
  # tests
  ## summary results
  expect_true(assertthat::has_name(x, "summary_results"))
  expect_equal(ncol(x$summary_results), 2)
  expect_equal(nrow(x$summary_results), 3)
  expect_equal(
    x$summary_results[[1]],
    c(
      "Total cost",
      paste0("Number of sites allocated to “", p$action_ids, "”")
    )
  )
  expect_equal(
    x$summary_results[[2]],
    c(
      sum(
        sol[[1]] * p$get_action_costs(p$action_ids[[1]]),
        sol[[2]] * p$get_action_costs(p$action_ids[[2]])
      ),
      sum(sol[[1]]),
      sum(sol[[2]])
    )
  )
  ## feature results
  expect_true(assertthat::has_name(x, "feature_results"))
  expect_equal(x$feature_results[[1]], p$feature_ids)
  expect_equal(
    x$feature_results[[2]],
    c(
      sum(
        sol[[1]] * p$get_consequences_for_feature(
          p$action_ids[[1]],
          p$feature_ids[[1]]
        )
      ),
      sum(
        sol[[1]] * p$get_consequences_for_feature(
          p$action_ids[[1]],
          p$feature_ids[[2]]
        )
      ),
      sum(
        sol[[1]] * p$get_consequences_for_feature(
          p$action_ids[[1]],
          p$feature_ids[[3]]
        )
      )
    )
  )
  expect_equal(
    x$feature_results[[3]],
    c(
      sum(
        sol[[2]] * p$get_consequences_for_feature(
          p$action_ids[[2]],
          p$feature_ids[[1]]
        )
      ),
      sum(
        sol[[2]] * p$get_consequences_for_feature(
          p$action_ids[[2]],
          p$feature_ids[[2]]
        )
      ),
      sum(
        sol[[2]] * p$get_consequences_for_feature(
          p$action_ids[[2]],
          p$feature_ids[[3]]
        )
      )
    )
  )
  expect_equal(
    x$feature_results[[4]],
    x$feature_results[[2]] + x$feature_results[[3]]
  )
  ## site results
  expect_true(assertthat::has_name(x, "site_results"))
  expect_equal(ncol(x$site_results), 4)
  expect_equal(nrow(x$site_results), 5)
  expect_equal(
    x$site_results[[1]],
    p$site_ids
  )
  expect_equal(
    x$site_results[[2]],
    p$get_current_status_data()[[1]]
  )
  expect_equal(
    x$site_results[[3]],
    vapply(seq_len(nrow(sol)), FUN.VALUE = character(1), function(i) {
      p$action_ids[[which(c(as.matrix(sol[i, ])) > 0.5)]]
    })
  )
  expect_equal(
    x$site_results[[4]],
    vapply(seq_len(nrow(sol)), FUN.VALUE = numeric(1), function(i) {
      a <- which(c(as.matrix(sol[i, ])) > 0.5)
      p$get_action_costs(p$action_ids[[a]])[[i]]
    })
  )
  ## budget
  expect_true(assertthat::has_name(x, "budget"))
  expect_equal(x$budget, 5.23)
})
