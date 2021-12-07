context("default_colors()")

test_that("single color", {
  # create object
  d <- letters[1]
  x <- default_colors(d)
  # tests
  expect_is(x, "character")
  expect_length(x, length(d))
  expect_named(x, d)
  expect_equal(anyDuplicated(x), 0L)
  expect_true(all(startsWith(x, "#")))
  expect_true(all(nchar(x) == 7))
})

test_that("multiple colors", {
  # create object
  d <- letters[3]
  x <- default_colors(d)
  # tests
  expect_is(x, "character")
  expect_length(x, length(d))
  expect_named(x, d)
  expect_equal(anyDuplicated(x), 0L)
  expect_true(all(startsWith(x, "#")))
  expect_true(all(nchar(x) == 7))
})

test_that("many colors", {
  # create object
  d <- as.character(seq_len(100))
  x <- default_colors(d)
  # tests
  expect_is(x, "character")
  expect_length(x, length(d))
  expect_named(x, d)
  expect_true(all(startsWith(x, "#")))
  expect_true(all(nchar(x) == 7))
})
