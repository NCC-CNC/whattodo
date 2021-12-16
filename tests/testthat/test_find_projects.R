context("find_projects")

test_that("singe user group", {
  # create object
  d <- system.file("extdata", "projects", package = "whattodo")
  x <- find_projects(d, user_group = "admin")
  # tests
  expect_is(x, "tbl_df")
  expect_named(x, c("path", "name", "status"))
  expect_equal(
    x$name,
    c("Simulated dataset")
  )
  expect_equal(
    basename(x$path),
    c("simulated-data.yaml")
  )
  expect_equal(x$status, rep(TRUE, nrow(x)))
})

test_that("multiple user groups", {
  # TODO
})
