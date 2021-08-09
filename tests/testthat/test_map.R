context("map")

test_that("initialize_map", {
  parameters <- app_parameters()
  d <- simulate_data(
    n_sites = 5, n_features = 3, n_actions = 4,  parameters = parameters)
  d$site_spatial_data$id <- paste0("F", seq_len(nrow(d$site_spatial_data)))
  x <- initialize_map(default_bbox(), d$site_spatial_data, parameters)
  expect_is(x, "leaflet")
})

test_that("update_map (POINT class spatial data)", {
  # data
  parameters <- app_parameters()
  d <- simulate_data(
    n_sites = 5, n_features = 3, n_actions = 4,  parameters = parameters)
  d$site_spatial_data$id <- paste0("F", seq_len(nrow(d$site_spatial_data)))
  m <- initialize_map(default_bbox(), d$site_spatial_data, parameters)
  r <- tibble::tibble(
    name = d$site_names,
    action = sample(d$action_names, length(d$site_names), replace = TRUE))
  names(r) <- c(
    parameters$site_results_sheet$name_header,
    parameters$site_results_sheet$action_header)
  # main processing
  out <- update_map(
    m, d$feature_names, d$site_spatial_data, r,
    d$action_expectation_data, default_colors(d$action_names, parameters),
    parameters, NULL)
  # tests
  expect_is(out , "leaflet")
})

test_that("update_map (MULTILINESTRING class spatial data)", {
  # data
  parameters <- app_parameters()
  d <- simulate_data(
    n_sites = 5, n_features = 3, n_actions = 4,  parameters = parameters)
  d$site_spatial_data$id <- paste0("F", seq_len(nrow(d$site_spatial_data)))
  m <- initialize_map(default_bbox(), d$site_spatial_data, parameters)
  r <- tibble::tibble(
    name = d$site_names,
    action = sample(d$action_names, length(d$site_names), replace = TRUE))
  names(r) <- c(
    parameters$site_results_sheet$name_header,
    parameters$site_results_sheet$action_header)
  # update geometries
  pts <- matrix(1:10, , 2)
  sf::st_geometry(d$site_spatial_data) <-
    sf::st_sfc(
      sf::st_multilinestring(list(pts)),
      sf::st_multilinestring(list(pts + 10)),
      sf::st_multilinestring(list(pts + 20)),
      sf::st_multilinestring(list(pts + 30)),
      sf::st_multilinestring(list(pts + 40)))
  # main processing
  out <- update_map(
    m, d$feature_names, d$site_spatial_data, r,
    d$action_expectation_data, default_colors(d$action_names, parameters),
    parameters, NULL)
  # tests
  expect_is(out , "leaflet")
})

test_that("update_map (MULTIPOLYGON class spatial data)", {
  # data
  parameters <- app_parameters()
  d <- simulate_data(
    n_sites = 5, n_features = 3, n_actions = 4,  parameters = parameters)
  d$site_spatial_data$id <- paste0("F", seq_len(nrow(d$site_spatial_data)))
  m <- initialize_map(default_bbox(), d$site_spatial_data, parameters)
  r <- tibble::tibble(
    name = d$site_names,
    action = sample(d$action_names, length(d$site_names), replace = TRUE))
  names(r) <- c(
    parameters$site_results_sheet$name_header,
    parameters$site_results_sheet$action_header)
  # update geometries
  outer <- matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
  hole1 <- matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
  hole2 <- matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
  sf::st_geometry(d$site_spatial_data) <-
    sf::st_sfc(
      sf::st_multipolygon(list(list(outer, hole1), list(outer))),
      sf::st_multipolygon(list(list(outer, hole1), list(outer + 20))),
      sf::st_multipolygon(list(list(outer, hole1), list(outer + 30))),
      sf::st_multipolygon(list(list(outer, hole1), list(outer + 40))),
      sf::st_multipolygon(list(list(outer, hole1), list(outer + 10))))
  # main processing
  out <- update_map(
    m, d$feature_names, d$site_spatial_data, r,
    d$action_expectation_data, default_colors(d$action_names, parameters),
    parameters, NULL)
  # tests
  expect_is(out , "leaflet")
})

test_that("update_map (GEOMETRY class spatial data)", {
  # data
  parameters <- app_parameters()
  d <- simulate_data(
    n_sites = 5, n_features = 3, n_actions = 4,  parameters = parameters)
  d$site_spatial_data$id <- paste0("F", seq_len(nrow(d$site_spatial_data)))
  m <- initialize_map(default_bbox(), d$site_spatial_data, parameters)
  r <- tibble::tibble(
    name = d$site_names,
    action = sample(d$action_names, length(d$site_names), replace = TRUE))
  names(r) <- c(
    parameters$site_results_sheet$name_header,
    parameters$site_results_sheet$action_header)
  # update geometries
  pts <- matrix(1:10, , 2)
  outer <- matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
  hole1 <- matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
  hole2 <- matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
  sf::st_geometry(d$site_spatial_data) <-
    sf::st_sfc(
      sf::st_multilinestring(list(pts)),
      sf::st_multipolygon(list(list(outer, hole1), list(outer + 20))),
      sf::st_multipolygon(list(list(outer, hole1), list(outer + 30))),
      sf::st_multipolygon(list(list(outer, hole1), list(outer + 40))),
      sf::st_multipolygon(list(list(outer, hole1), list(outer + 10))))
  # main processing
  out <- update_map(
    m, d$feature_names, d$site_spatial_data, r,
    d$action_expectation_data, default_colors(d$action_names, parameters),
    parameters, NULL)
  # tests
  expect_is(out , "leaflet")
})
