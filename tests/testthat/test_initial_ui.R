context("initial_ui")

test_that("initial_data_ui", {
  expect_is(initial_data_ui(), "shiny.tag.list")
})

test_that("initial_results_ui", {
  expect_is(initial_results_ui(), "shiny.tag.list")
})

test_that("initial_map_ui", {
  expect_is(initial_map_ui(), "shiny.tag.list")
})

test_that("initial_sidebar_ui", {
  # create ui
  x <- initial_sidebar_ui()
  # tests
  expect_is(x, "shiny.tag.list")
  expect_true(has_html_node(x, "spreadsheet_upload_widget"))
  expect_true(has_html_node(x, "shapefile_query_widget"))
  expect_true(has_html_node(x, "shapefile_upload_widget"))
  expect_true(has_html_node(x, "data_done_btn"))
  expect_true(has_html_node(x, "data_done_btn", "action-button"))
})
