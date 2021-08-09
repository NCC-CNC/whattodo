context("defaults")

test_that("default_tabular_data", {
  expect_is(default_tabular_data(), "data.frame")
})

test_that("default_bbox", {
  expect_is(default_bbox(), "bbox")
})

test_that("default_colors (small number of colors)", {
  n <- 5
  d <- default_colors(letters[seq_len(n)], app_parameters())
  expect_true(inherits(d[[1]], "character"))
  expect_true(inherits(d, "matrix"))
  expect_equal(ncol(d), n)
  expect_false(any(duplicated(d[1, ])))
  expect_false(any(duplicated(d[2, ])))
  expect_equal(colnames(d), letters[seq_len(n)])
})

test_that("default_colors (absurd number of colors)", {
  n <- 1000
  d <- default_colors(as.character(seq_len(n)), app_parameters())
  expect_true(inherits(d[[1]], "character"))
  expect_true(inherits(d, "matrix"))
  expect_equal(ncol(d), n)
  expect_true(any(duplicated(d[1, ])))
  expect_true(any(duplicated(d[2, ])))
  expect_equal(colnames(d), as.character(seq_len(n)))
})
