context("primary user interfaces")

test_that("primary_data_ui", {
  # set variables for making ui
  site_names <- paste0("s", seq_len(5))
  feature_names <- paste0("f", seq_len(4))
  action_names <- paste0("a", seq_len(4))
  parameters <- app_parameters()

  # create ui
  x <- primary_data_ui(
    site_names = site_names, feature_names = feature_names,
    action_names = action_names, parameters = parameters
  )

  # tests
  expect_is(x, "shiny.tag.list")
  expect_true(has_html_node(x, "site_data_widget"))
  expect_true(has_html_node(x, "site_data_widget", "rhandsontable"))
  expect_true(has_html_node(x, "site_status_widget"))
  expect_true(has_html_node(x, "site_status_widget", "rhandsontable"))
  expect_true(has_html_node(x, "feature_data_widget"))
  expect_true(has_html_node(x, "feature_data_widget", "rhandsontable"))
  for (i in seq_along(action_names)) {
    expect_true(has_html_node(x, paste0("action_", i, "_widget")))
    expect_true(has_html_node(
      x,
      paste0("action_", i, "_widget"), "rhandsontable"
    ))
  }
})

test_that("primary_results_ui", {
  # set variables for making ui
  site_names <- paste0("s", seq_len(5))
  feature_names <- paste0("f", seq_len(4))
  action_names <- paste0("a", seq_len(4))
  parameters <- app_parameters()

  # create ui
  x <- primary_results_ui(
    site_names = site_names,
    feature_names = feature_names,
    action_names = action_names, parameters = parameters
  )

  # tests
  expect_is(x, "shiny.tag.list")
  expect_true(has_html_node(x, "summary_results_widget"))
  expect_true(has_html_node(x, "summary_results_widget", "rhandsontable"))
  expect_true(has_html_node(x, "site_results_widget"))
  expect_true(has_html_node(x, "site_results_widget", "rhandsontable"))
  expect_true(has_html_node(x, "feature_results_widget"))
  expect_true(has_html_node(x, "feature_results_widget", "rhandsontable"))
})

test_that("primary_sidebar_ui", {
  # set variables for making ui
  site_names <- paste0("s", seq_len(5))
  feature_names <- paste0("f", seq_len(4))
  action_names <- paste0("a", seq_len(4))
  parameters <- app_parameters()

  # create ui
  x <- primary_sidebar_ui(
    site_names = site_names,
    feature_names = feature_names,
    action_names = action_names, parameters = parameters
  )

  # tests
  expect_is(x, "shiny.tag")
  expect_true(has_html_node(x, "problem_widget"))
  expect_true(has_html_node(x, "budget_widget"))
  expect_true(has_html_node(x, "solution_btn"))
  expect_true(has_html_node(x, "solution_btn", "action-button"))
  expect_true(has_html_node(x, "export_btn"))
  expect_true(has_html_node(x, "export_btn", "shiny-download-link"))
})

test_that("primary_map_ui", {
  # set variables for making ui
  site_names <- paste0("s", seq_len(5))
  feature_names <- paste0("f", seq_len(4))
  action_names <- paste0("a", seq_len(4))
  parameters <- app_parameters()

  # create ui
  x <- primary_map_ui(
    site_names = site_names,
    feature_names = feature_names,
    action_names = action_names, parameters = parameters
  )

  # tests
  expect_is(x, "shiny.tag.list")
  expect_true(has_html_node(x, "map_widget", "leaflet"))
})
