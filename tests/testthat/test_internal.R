context("internal")

test_that("st_geometry_data_type", {
  # data (from sf examples)
  pts <- matrix(seq_len(10), , 2)
  outer <- matrix(c(0, 0, 10, 0, 10, 10, 0, 10, 0, 0), ncol = 2, byrow = TRUE)
  hole1 <- matrix(c(1, 1, 1, 2, 2, 2, 2, 1, 1, 1), ncol = 2, byrow = TRUE)
  hole2 <- matrix(c(5, 5, 5, 6, 6, 6, 6, 5, 5, 5), ncol = 2, byrow = TRUE)
  s1 <- sf::st_as_sf(d = 1, sf::st_sfc(sf::st_point(c(1, 2))))
  s2 <- sf::st_as_sf(d = 1, sf::st_sfc(sf::st_linestring(pts)))
  s3 <- sf::st_as_sf(d = 1, sf::st_sfc(sf::st_polygon(list(outer))))
  s4 <- sf::st_as_sf(d = 1, sf::st_sfc(sf::st_multipolygon(
    list(list(outer, hole1), list(outer))
  )))
  s5 <- sf::st_as_sf(d = 1, sf::st_sfc(sf::st_multilinestring(
    list(pts, pts + 3)
  )))
  # tests
  expect_equal(st_geometry_data_type(s1), "POINT")
  expect_equal(st_geometry_data_type(s2), "LINESTRING")
  expect_equal(st_geometry_data_type(s3), "POLYGON")
  expect_equal(st_geometry_data_type(s4), "MULTIPOLYGON")
  expect_equal(st_geometry_data_type(s5), "MULTILINESTRING")
})

test_that("paste_vector", {
  expect_equal(paste_vector("a"), "\"a\"")
  expect_equal(paste_vector(c("a", "b")), "\"a\", \"b\"")
})

test_that("column_widths", {
  w <- column_widths(iris[, 1:3])
  expect_is(w, "numeric")
  expect_length(w, 3)
  expect_true(all(w > 0))
})
