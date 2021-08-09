#' Simulate data
#'
#' @param n_sites `integer` number of sites.
#'
#' @param n_features `integer` number of features.
#'
#' @param n_actions `integer` number of actions.
#'
#' @param parameters `list` object containing configuration parameters.
#'
#' @param prop_locked_out `numeric` proportion of locked out actions. Defaults
#'   to 0.05 (5%).
#'
#' @return `list` object containing the simulated data.
simulate_data <- function(n_sites, n_features, n_actions, parameters, prop_locked_out = 0.05) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.count(n_sites),
    assertthat::is.count(n_features),
    assertthat::is.count(n_actions),
    is.list(parameters),
    assertthat::is.number(prop_locked_out)
  )
  # simulate names
  site_names <- paste("site", seq_len(n_sites))
  feature_names <- paste("feature", seq_len(n_features))
  action_names <- paste("action", seq_len(n_actions))

  # simulate site data
  site_data <- tibble::tibble(
    name = site_names,
    x = runif(n_sites),
    y = runif(n_sites)
  )
  names(site_data) <- c(
    parameters$site_data_sheet$name_header,
    parameters$site_data_sheet$longitude_header,
    parameters$site_data_sheet$latitude_header
  )
  cost_data <- matrix(
    runif(n_sites * n_actions) * 5,
    nrow = n_sites, ncol = n_actions
  )
  cost_data <- tibble::as_tibble(as.data.frame(cost_data))
  names(cost_data) <- as.character(glue::glue(
    parameters$site_data_sheet$action_cost_header,
    action_names = action_names
  ))
  site_data <- dplyr::bind_cols(site_data, cost_data)

  # simulate site spatial data
  site_spatial_data <- site_data[, 1:3]
  names(site_spatial_data) <- c("name", "x", "y")
  site_spatial_data$id <- paste0("F", seq_len(nrow(site_data)))
  site_spatial_data <- sf::st_as_sf(
    site_spatial_data,
    coords = c("x", "y"), crs = 4326
  )

  # simulate site status data
  site_status_data <- tibble::tibble(name = site_names)
  status_data <- matrix(1, nrow = n_sites, ncol = n_actions)
  zero_idx <- sample.int(
    length(status_data), floor(length(status_data) * prop_locked_out)
  )
  status_data[zero_idx] <- 0
  status_data <- tibble::as_tibble(as.data.frame(status_data))
  names(status_data) <- as.character(glue::glue(
    parameters$site_status_sheet$action_status_header,
    action_names = action_names
  ))
  site_status_data <- dplyr::bind_cols(site_status_data, status_data)

  # simulate feature data
  feature_data <- tibble::tibble(
    name = feature_names,
    target = runif(n_features, 0.4, 0.6) * n_sites,
    weight = runif(n_features, 0.5, 1.0)
  )
  names(feature_data) <- c(
    parameters$feature_data_sheet$name_header,
    parameters$feature_data_sheet$target_header,
    parameters$feature_data_sheet$weight_header
  )

  # simulate action expectation data
  cn <- as.character(glue::glue(
    parameters$action_expectation_sheet$action_expectation_header,
    feature_names = feature_names
  ))
  action_expectation_data <- lapply(seq_along(action_names), function(i) {
    s <- tibble::tibble(name = site_names)
    v <- matrix(runif(n_sites * n_features), nrow = n_sites, ncol = n_features)
    v <- tibble::as_tibble(as.data.frame(v))
    names(v) <- cn
    names(s) <- parameters$site_data_sheet$name_header
    dplyr::bind_cols(s, v)
  })

  # return result
  list(
    site_names = site_names,
    feature_names = feature_names,
    action_names = action_names,
    site_data = site_data,
    site_spatial_data = site_spatial_data,
    site_status_data = site_status_data,
    feature_data = feature_data,
    action_expectation_data = action_expectation_data
  )
}
