#' @include internal.R
NULL

#' Project class
#'
#' Definition for the Project class.
#'
#' @seealso [new_project()].
Project <- R6::R6Class(
  "Project",
  public = list(

    #' @field id `character` identifier.
    id = NA_character_,

    #' @field site_ids `character` vector
    site_ids = NA_character_,

    #' @field site_descriptions `character` vector
    site_descriptions = NA_character_,

    #' @field feature_ids `character` vector
    feature_ids = NA_character_,

    #' @field feature_html_ids `character` vector
    feature_html_ids = NA_character_,

    #' @field feature_descriptions `character` vector
    feature_descriptions = NA_character_,

    #' @field action_ids `character` vector
    action_ids = NA_character_,

    #' @field action_colors `character` vector of colors for each action.
    action_colors = NULL,

    #' @field action_descriptions `character` vector
    action_descriptions = NA_character_,

    #' @field site_data `data.frame` object.
    site_data = NULL,

    #' @field feature_data `data.frame` object.
    feature_data = NULL,

    #' @field feasibility_data `data.frame` object.
    feasibility_data = NULL,

    #' @field site_geometry [sf::st_sf()] object.
    site_geometry = NULL,

    #' @field parameters `list` of parameters.
    parameters = list(),

    #' @field settings `list` of `Parameter` objects.
    settings = list(),

    #' @field consequence_data `list` of `data.frame` objects.
    consequence_data = NULL,

    #' @field site_cost_headers `character` vector with names.
    site_cost_headers = NA_character_,

    #' @field site_status_header `character` value.
    site_status_header = NA_character_,

    #' @field action_feasibility_headers `character` vector with names.
    action_feasibility_headers = NA_character_,

    #' @field feature_goal_header `character` value.
    feature_goal_header = NA_character_,

    #' @field feature_weight_header `character` value.
    feature_weight_header = NA_character_,

    #' @field consequence_action_headers `character` named vector.
    consequence_action_headers = NA_character_,

    #' @field consequence_feature_headers `character` named vector.
    consequence_feature_headers = NA_character_,

    #' @description
    #' Create a Project object.
    #' @param id `character` value.
    #' @param site_ids `character` vector
    #' @param site_descriptions `character` vector
    #' @param site_geometry [sf::st_sf()] object.
    #' @param feature_ids `character` vector
    #' @param feature_descriptions `character` vector
    #' @param action_ids `character` vector
    #' @param action_descriptions `character` vector
    #' @param site_data `data.frame` object.
    #' @param feature_data `data.frame` object.
    #' @param feasibility_data `data.frame` object.
    #' @param consequence_data `list` of `data.frame` objects.
    #' @param parameters `list` of parameters.
    #' @return A new Project object.
    initialize = function(id,
                          site_ids, site_descriptions, site_geometry,
                          feature_ids, feature_descriptions,
                          action_ids, action_descriptions,
                          site_data,
                          feature_data,
                          feasibility_data,
                          consequence_data,
                          parameters) {
      # assert that arguments are valid
      assertthat::assert_that(
        ## ids
        assertthat::is.string(id),
        assertthat::noNA(id),
        is.character(site_ids),
        assertthat::noNA(site_ids),
        is.character(feature_ids),
        assertthat::noNA(feature_ids),
        is.character(action_ids),
        assertthat::noNA(action_ids),
        ## descriptions
        is.character(site_descriptions),
        assertthat::noNA(site_descriptions),
        is.character(feature_descriptions),
        assertthat::noNA(feature_descriptions),
        is.character(action_descriptions),
        assertthat::noNA(action_descriptions),
        identical(length(site_ids), length(site_descriptions)),
        identical(length(feature_ids), length(feature_descriptions)),
        identical(length(action_ids), length(action_descriptions)),
        ## data
        inherits(site_data, "data.frame"),
        inherits(feasibility_data, "data.frame"),
        inherits(feature_data, "data.frame"),
        inherits(consequence_data, "list"),
        ## parameters
        is.list(parameters),
        ## geometry
        inherits(site_geometry, "sf")
      )

      # assign data fields
      self$id <- id
      self$site_ids <- site_ids
      self$site_descriptions <- site_descriptions
      self$feature_ids <- feature_ids
      self$feature_html_ids <- stats::setNames(
        convert_to_html_id(feature_ids), feature_ids
      )
      self$feature_descriptions <- feature_descriptions
      self$action_ids <- action_ids
      self$action_descriptions <- action_descriptions
      self$site_data <- site_data
      self$feature_data <- feature_data
      self$feasibility_data <- feasibility_data
      self$consequence_data <- consequence_data
      self$parameters <- parameters
      self$site_geometry <- site_geometry

      # assign fields derived from parameters
      ## site data headers
      self$site_cost_headers <- stats::setNames(
        glue::glue(
          parameters$site_data_sheet$action_cost_header,
          action_ids = action_ids
        ),
        action_ids
      )
      self$site_status_header <- parameters$site_data_sheet$status_header
      ## action data headers
      self$action_feasibility_headers <- stats::setNames(
        glue::glue(
          parameters$feasibility_data_sheet$action_feasibility_header,
          action_ids = action_ids
        ),
        action_ids
      )
      ## feature data headers
      self$feature_goal_header <- parameters$feature_data_sheet$goal_header
      self$feature_weight_header <- parameters$feature_data_sheet$weight_header
      ## consequence data headers
      self$consequence_feature_headers <- stats::setNames(
        glue::glue(
          parameters$consequence_sheet$consequence_header,
          feature_ids = feature_ids
        ),
        feature_ids
      )
      self$consequence_action_headers <- stats::setNames(
        glue::glue(
          parameters$consequence_sheet$sheet_name,
          action_ids = action_ids
        ),
        action_ids
      )

      # assign additional fields
      self$action_colors <- default_colors(action_ids)

      # add field for total budget setting
      ## compute values
      mn <- self$get_min_budget()
      mx <- self$get_max_budget()
      ## create settings
      self$settings <- list(
        new_parameter(
          name = "Total budget",
          status = FALSE,
          value = mn,
          min_value = mn,
          max_value = mx,
          step_value = 1,
          units = "",
          hide = TRUE,
          id = "budget_parameter"
        )
      )
    },

    #' @description
    #' Print the object.
    #' @param ... not used.
    print = function(...) {
      message("Project")
      message("  id:       ", self$id)
      message("  sites:    ", paste_vector(self$site_ids))
      message("  features: ", paste_vector(self$feature_ids))
      message("  actions:  ", paste_vector(self$action_ids))
      message("  geometry: ", inherits(self$site_geometry, "sf"))
      invisible(self)
    },

    #' @description
    #' Generate a `character` summarizing the representation of the object.
    #' @return `character` value.
    repr = function() {
      paste0("Project (", self$id, ")")
    },

    #' @description
    #' Get site identifiers.
    #' @return `character` vector.
    get_site_ids = function() {
      self$site_ids
    },

    #' @description
    #' Get feature identifiers.
    #' @return `character` vector.
    get_feature_ids = function() {
      self$feature_ids
    },

    #' @description
    #' Get feature HTML identifiers.
    #' @param feature_html_id `character` vector of feature identifiers.
    #' @return `character` vector.
    get_feature_ids_from_html_id = function(feature_html_id) {
      assertthat::assert_that(
        is.character(feature_html_id),
        assertthat::noNA(feature_html_id),
        all(feature_html_id %in% self$feature_html_ids)
      )
      i <- match(feature_html_id, self$feature_html_ids)
      names(self$feature_html_ids[i])
    },

    #' @description
    #' Get action identifiers.
    #' @return `character` vector.
    get_action_ids = function() {
      self$action_ids
    },

    #' @description
    #' Get feature goal.
    #' @param feature_id `character` identifier for feature
    #' @return `numeric` value.
    get_feature_goal = function(feature_id) {
      assertthat::assert_that(
        assertthat::is.string(feature_id),
        assertthat::noNA(feature_id),
        feature_id %in% self$feature_ids
      )
      i <- match(feature_id, self$feature_ids)
      self$feature_data[[self$feature_goal_header]][[i]]
    },

    #' @description
    #' Get feature weight.
    #' @param feature_id `character` identifier for feature
    #' @return `numeric` value.
    get_feature_weight = function(feature_id) {
      assertthat::assert_that(
        assertthat::is.string(feature_id),
        assertthat::noNA(feature_id),
        feature_id %in% self$feature_ids
      )
      i <- match(feature_id, self$feature_ids)
      self$feature_data[[self$feature_weight_header]][[i]]
    },

    #' @description
    #' Get colors for actions.
    #' @param action_id `character` identifier for action.
    #' @return `character` vector.
    get_site_statuses = function() {
      self$site_data[[self$site_status_header]]
    },

    #' @description
    #' Get cost data for implementing an action within each site.
    #' @param action_id `character` identifier for action.
    #' @return `numeric` vector.
    get_action_costs = function(action_id) {
      assertthat::assert_that(
        assertthat::is.string(action_id),
        assertthat::noNA(action_id),
        action_id %in% self$action_ids
      )
      self$site_data[[self$site_cost_headers[[action_id]]]]
    },

    #' @description
    #' Get feasibility data for implementing an action within each site.
    #' @param action_id `character` identifier for action.
    #' @return `logical` vector.
    get_action_feasibility = function(action_id) {
      assertthat::assert_that(
        assertthat::is.string(action_id),
        assertthat::noNA(action_id),
        action_id %in% self$action_ids
      )
      self$feasibility_data[[self$action_feasibility_headers[[action_id]]]]
    },

    #' @description
    #' Get consequence data for features.
    #' @param action_id `character` identifier for action.
    #' @param feature_id `character` identifier for feature.
    #' @return `numeric` vector.
    get_consequences_for_feature = function(action_id, feature_id) {
      assertthat::assert_that(
        assertthat::is.string(action_id),
        assertthat::noNA(action_id),
        action_id %in% self$action_ids,
        assertthat::is.string(feature_id),
        assertthat::noNA(feature_id),
        feature_id %in% self$feature_ids
      )
      i <- match(action_id, self$action_ids)
      j <- self$consequence_feature_headers[[feature_id]]
      self$consequence_data[[i]][[j]]
    },

    #' @description
    #' Get the cost of the cheapest solution.
    #' @return `numeric` value.
    get_min_budget = function() {
      sum(
        apply(
          X = as.matrix(
            self$site_data[, unname(self$site_cost_headers), drop = FALSE]
          ),
          MARGIN = 1,
          FUN = min,
          na.rm = TRUE
        )
      )
    },

    #' @description
    #' Get the cost of the most expensive solution.
    #' @return `numeric` value.
    get_max_budget = function() {
      sum(
        apply(
          X = as.matrix(
            self$site_data[, unname(self$site_cost_headers), drop = FALSE]
          ),
          MARGIN = 1,
          FUN = max,
          na.rm = TRUE
        )
      )
    },

    #' @description
    #' Get the highest amount for each feature.
    #' @return `data.frame` containing a `feature_id` and `amount` columns.
    get_max_feature_consequence = function() {
      tibble::tibble(
        feature_id = self$feature_ids,
        amount = vapply(
          seq_along(self$feature_ids), FUN.VALUE = numeric(1), function(i) {
            ii <- self$consequence_feature_headers[[i]]
            m <- vapply(
              seq_along(self$action_ids),
              FUN.VALUE = numeric(length(self$site_ids)),
              function(j) self$consequence_data[[j]][[ii]]
            )
            sum(apply(m, 1, max, na.rm = TRUE))
          }
        )
      )
    },

    #' @description
    #' Get the current expected amount for each feature.
    #' @return `data.frame` containing a `feature_id` and `amount` columns.
    get_current_feature_consequence = function() {
      ss <- self$get_site_statuses()
      tibble::tibble(
        feature_id = self$feature_ids,
        amount = vapply(
          self$feature_ids,
          FUN.VALUE = numeric(1),
          function(i) {
            sum(
              vapply(
                seq_along(self$site_ids),
                FUN.VALUE = numeric(1),
                function(j) {
                  self$get_consequences_for_feature(ss[[j]], i)[[j]]
                }
              )
            )
          }
        )
      )
    },

    #' @description
    #' Get layer names for rendering data on map.
    #' @return `character` vector of layer names.
    get_map_layers = function() {
      d <- expand.grid(x = self$action_ids, y = self$feature_ids)
      stats::setNames(
        object = c(
          "location",
          "status",
          glue::glue("cost_{x}", x = self$action_ids),
          glue::glue("feasibility_{x}", x = self$action_ids),
          glue::glue("consequence_{x}_{y}", x = d$x, y = d$y)
        ),
        nm = c(
          "Location",
          self$parameters$site_data_sheet$status_header,
          glue::glue(
            self$parameters$site_data_sheet$action_cost_header,
            action_ids = self$action_ids
          ),
          glue::glue(
            self$parameters$feasibility_data_sheet$action_feasibility_header,
            action_ids = self$action_ids
          ),
          paste0(
            glue::glue(
              self$parameters$consequence_sheet$sheet_name,
              action_ids = d$x,
            ),
            ": ",
            glue::glue(
              self$parameters$consequence_sheet$consequence_header,
              feature_ids = d$y,
            )
          )
        )
      )
    },

    #' @description
    #' Get planning unit data for optimization.
    #' @return `data.frame` object.
    get_pu_data = function() {
      ## extract cost
      cost_data <- self$site_data[, self$site_cost_headers, drop = FALSE]
      ## extract consequence data
      consequence_data <- lapply(seq_along(self$action_ids), function(i) {
        out <- self$consequence_data[[i]]
        out <- out[, self$consequence_feature_headers, drop = FALSE]
        names(out) <- paste0(self$action_ids[i], "_", self$feature_ids)
        out
      })
      ## return result
      do.call(
        dplyr::bind_cols,
        append(
          list(tibble::tibble(site = self$site_ids), cost_data),
          consequence_data
        )
      )
    },

    #' @description
    #' Get zone data for optimization.
    #' @return `data.frame` object.
    get_zone_data = function() {
      ## create zone name column
      args <- lapply(
        self$action_ids, function(x) paste0(x, "_", self$feature_ids)
      )
      ## append feature and zone names to object
      args <- append(
        args,
        list(zone_names = self$action_ids, feature_names = self$feature_ids)
      )
      ## return zones object
      do.call(prioritizr::zones, args)
    },

    #' @description
    #' Get goal data for optimization.
    #' @return `data.frame` object.
    get_goal_data = function() {
      tibble::tibble(
        feature = self$feature_ids,
        zone = list(self$action_ids)[rep(1, length(self$feature_ids))],
        type = "absolute",
        sense = ">=",
        target = c(
          (self$feature_data[[self$feature_goal_header]] / 100) *
          self$get_max_feature_consequence()$amount
        )
      )
    },

    #' @description
    #' Get weight data for optimization.
    #' @return `data.frame` object.
    get_weight_data = function() {
      tibble::tibble(
        feature = self$feature_ids,
        weight = self$feature_data[[self$feature_weight_header]]
      )
    },

    #' @description
    #' Get locked data for optimization.
    #' @return `data.frame` object.
    get_locked_data = function() {
      # prepare data
      d <- self$feasibility_data
      names(d) <- c("site", self$action_ids)
      d <- tidyr::gather(d, action, status, -site)
      d <- d[d$status <= 0.5, , drop = FALSE]
      d$id <- match(d$site, self$site_ids)
      assertthat::assert_that(assertthat::noNA(d$id))
      # return result
      tibble::tibble(pu = d$id, zone = d$action, status = 0)
    },

    #' @description
    #' Get the bounding box.
    #' @param expand `FALSE` should the bounding box be expanded by 10%?
    #' @return `list` object with `"xmin"`, `"xmax"`, `"ymin"`, and `"ymax"`
    #'   elements.
    get_bbox = function(expand = FALSE) {
      # assert arguments are valid
      assertthat::assert_that(
        assertthat::is.flag(expand),
        assertthat::noNA(expand)
      )
      # generate extent object
      ext <- as.list(sf::st_bbox(self$site_geometry))
      # expand bounding box if needed
      if (expand) {
        out <- list()
        out$xmin <- unname(ext$xmin - (0.1 * (ext$xmax - ext$xmin)))
        out$xmax <- unname(ext$xmax + (0.1 * (ext$xmax - ext$xmin)))
        out$ymin <- unname(ext$ymin - (0.1 * (ext$ymax - ext$ymin)))
        out$ymax <- unname(ext$ymax + (0.1 * (ext$ymax - ext$ymin)))
      } else {
        out <- list(
          xmin = unname(ext$xmin),
          xmax = unname(ext$xmax),
          ymin = unname(ext$ymin),
          ymax = unname(ext$ymax)
        )
      }
      # if using lon/lat CRS, then ensure valid extent
      out$xmin <- max(out$xmin, -180)
      out$xmax <- min(out$xmax, 180)
      out$ymin <- max(out$ymin, -90)
      out$ymax <- min(out$ymax, 90)
      # return result
      out
    },

    #' @description
    #' Get data for rendering the solution settings widget.
    #' @return `list` object.
    get_solution_settings_data = function() {
      list(
        themes = self$get_goals_settings_data(),
        weights = self$get_weights_settings_data(),
        parameters = lapply(self$settings, function(x) x$get_widget_data())
      )
    },

    #' @description
    #' Get data for rendering widget to specify goals.
    #' @return `list` object.
    get_goals_settings_data = function() {
      # extract column names from parameters
      nh <- self$parameters$feature_data_sheet$name_header
      th <- self$parameters$feature_data_sheet$goal_header
      mx <- self$get_max_feature_consequence()
      cr <- self$get_current_feature_consequence()
      # generate data
      lapply(seq_len(nrow(self$feature_data)), function(i) {
        list(
          id = paste0("T", self$feature_html_ids[[i]]),
          name = self$feature_ids[[i]],
          feature_name = self$feature_ids[[i]],
          feature_id = paste0("F", self$feature_html_ids[[i]]),
          feature_total_amount = mx$amount[[i]],
          feature_current_held = cr$amount[[i]] / mx$amount[[i]],
          feature_min_goal = 0,
          feature_max_goal = 1,
          feature_goal = self$feature_data[[th]][[i]] / 100,
          feature_step_goal = 0.01,
          units = "units"
        )
      })
    },

    #' @description
    #' Get data for rendering widget to specify weights.
    #' @return `list` object.
    get_weights_settings_data = function() {
      # extract column names from parameters
      nh <- self$parameters$feature_data_sheet$name_header
      wh <- self$parameters$feature_data_sheet$weight_header
      # generate data
      lapply(seq_len(nrow(self$feature_data)), function(i) {
        list(
          id = paste0("W", self$feature_html_ids[[i]]),
          name = self$feature_data[[nh]][[i]],
          min_factor = 0,
          max_factor = 100,
          factor = self$feature_data[[wh]][[i]],
          step_factor = 1
        )
      })
    },

    #' @description
    #' Set goal for a feature.
    #' @param feature_id `character` value containing the feature identifier.
    #' @param value `numeric` value containing the new goal.
    set_feature_goal = function(feature_id, value) {
      assertthat::assert_that(
        assertthat::is.string(feature_id),
        assertthat::noNA(feature_id),
        isTRUE(feature_id %in% self$feature_ids),
        assertthat::is.number(value),
        assertthat::noNA(value)
      )
      i <- which(self$feature_ids == feature_id)
      self$feature_data[[self$feature_goal_header]][[i]] <- value
      invisible(self)
    },

    #' @description
    #' Set weight for a feature.
    #' @param feature_id `character` value containing the feature identifier.
    #' @param value `numeric` value containing the new weight.
    set_feature_weight = function(feature_id, value) {
      assertthat::assert_that(
        assertthat::is.string(feature_id),
        assertthat::noNA(feature_id),
        isTRUE(feature_id %in% self$feature_ids),
        assertthat::is.number(value),
        assertthat::noNA(value)
      )
      i <- which(self$feature_ids == feature_id)
      self$feature_data[[self$feature_weight_header]][[i]] <- value
      invisible(self)
    },

    #' @description
    #' Set site data.
    #' @param x `data.frame` containing new data.
    set_site_data = function(x) {
      assertthat::assert_that(
        inherits(x, "data.frame"),
        identical(names(x), names(self$site_data))
      )
      self$site_data <- tibble::as_tibble(x)
      self$site_data <- dplyr::mutate_if(
        self$site_data, is.factor, as.character
      )
      self$settings[[1]]$min_value <- self$get_min_budget()
      self$settings[[1]]$max_value <- self$get_max_budget()
      invisible(self)
    },

    #' @description
    #' Set feature data.
    #' @param x `data.frame` containing new data.
    set_feature_data = function(x) {
      assertthat::assert_that(
        inherits(x, "data.frame"),
        identical(names(x), names(self$feature_data))
      )
      self$feature_data <- tibble::as_tibble(x)
      invisible(self)
    },

    #' @description
    #' Set feasibility data.
    #' @param x `data.frame` containing new data.
    set_feasibility_data = function(x) {
      assertthat::assert_that(
        inherits(x, "data.frame"),
        identical(names(x), names(self$feasibility_data))
      )
      self$feasibility_data <- tibble::as_tibble(x)
      invisible(self)
    },

    #' @description
    #' Set consequence data.
    #' @param x `data.frame` containing new data.
    #' @param action_id `character` identifier for action.
    set_consequence_data = function(x, action_id) {
      assertthat::assert_that(
        assertthat::is.string(action_id),
        assertthat::noNA(action_id),
        action_id %in% self$action_ids
      )
      i <- match(action_id, self$action_ids)
      assertthat::assert_that(
        inherits(x, "data.frame"),
        identical(names(x), names(self$consequence_data[[i]]))
      )
      self$consequence_data[[i]] <- tibble::as_tibble(x)
      invisible(self)
    },

    #' @description
    #' Render on map.
    #' @param map [leaflet::leaflet()] object.
    #' @param data `character` name of dataset to show.
    #'   Argument must be a valid layer name (see `self$get_map_layers()`).
    #' @param group `character` group name. Defaults to `"sites"`.
    #' @return [leaflet::leaflet()] map.
    render_on_map = function(map, data = "location", group = "sites") {
      # assert that arguments are valid
      assertthat::assert_that(
        inherits(map, c("leaflet", "leaflet_proxy")),
        assertthat::is.string(data),
        assertthat::noNA(data),
        assertthat::is.string(group),
        assertthat::noNA(group)
      )

      # clear map
      map <- leaflet::clearShapes(map)

      # prepare data for map
      ## display location
      if (identical(data, "location")) {
        pal <- leaflet::colorFactor(
          palette = default_colors("id")[[1]],
          domain = "Sites",
        )
        vals <- rep("Sites", length(self$site_ids))
        popups <- data.frame(tibble::tibble(name = self$site_ids))
        names(popups) <- self$parameters$site_data_sheet$name_header
      ## display status
      } else if (identical(data, "status")) {
        pal <- leaflet::colorFactor(
          palette = unname(self$action_colors),
          domain = self$action_ids,
          levels = self$action_ids
        )
        popups <- stats::setNames(
          object = tibble::tibble(
            name = self$site_ids,
            action = self$site_data[[4]]
          ),
          nm = c(
            self$parameters$site_data_sheet$name_header,
            self$parameters$site_data_sheet$status_header
          )
        )
        vals <- popups[[2]]
      ## display cost
    } else if (startsWith(data, "cost")) {
        ### extract id
        action_id <- unglue::unglue_vec(data, "cost_{x}")
        ### validate id
        assertthat::assert_that(
          assertthat::is.string(action_id),
          assertthat::noNA(action_id),
          isTRUE(action_id %in% self$action_ids)
        )
        ### prepare data
        v <- self$get_action_costs(action_id)
        pal <- leaflet::colorNumeric(
          palette = "inferno",
          domain = range(v)
        )
        popups <- stats::setNames(
          object = tibble::tibble(name = self$site_ids, x = v),
          nm = c(
            self$parameters$site_data_sheet$name_header,
            glue::glue(
              self$parameters$site_data_sheet$action_cost_header,
              action_ids = action_id
            )
          )
        )
        vals <- popups[[2]]
      ## display feasibility
      } else if (startsWith(data, "feasibility")) {
        ### extract id
        action_id <- unglue::unglue_vec(data, "feasibility_{x}")
        ### valid id
        assertthat::assert_that(
          assertthat::is.string(action_id),
          assertthat::noNA(action_id),
          isTRUE(action_id %in% self$action_ids)
        )
        ### prepare data
        pal <- leaflet::colorFactor(
          palette = c(
            self$parameters$true_style$bgFill,
            self$parameters$false_style$bgFill
          ),
          domain = c("feasible", "infeasible")
        )
        popups <- stats::setNames(
          object = tibble::tibble(
            name = self$site_ids,
            x = dplyr::if_else(
              self$get_action_feasibility(action_id) > 0.5,
              "feasible", "infeasible"
            )
          ),
          nm = c(
            self$parameters$site_data_sheet$name_header,
            glue::glue(
              self$parameters$feasibility_data_sheet$action_feasibility_header,
              action_ids = action_id
            )
          )
        )
        vals <- popups[[2]]
      ## display consequence data
      } else if (startsWith(data, "consequence")) {
        ### extract ids
        d <- unglue::unglue_data(data, pattern = "consequence_{x}_{y}")
        action_id <- d$x
        feature_id <- d$y
        ### validate ids
        assertthat::assert_that(
          assertthat::is.string(action_id),
          assertthat::is.string(feature_id),
          isTRUE(action_id %in% self$action_ids),
          isTRUE(feature_id %in% self$feature_ids)
        )
        v <- self$get_consequences_for_feature(action_id, feature_id)
        pal <- leaflet::colorNumeric(
          palette = "viridis",
          domain = range(v)
        )
        h <- self$parameters$consequence_sheet$consequence_header
        popups <- stats::setNames(
          object = tibble::tibble(name = self$site_ids, value = v),
          nm = c(
            self$parameters$site_data_sheet$name_header,
            glue::glue(h, feature_ids = feature_id)
          )
        )
        vals <- popups[[2]]
      } else {
        stop("invalid argument to data")
      }

      # clear group from map
      map <- leaflet::clearGroup(map, group)
      map <- leaflet::removeControl(map, "legend")

      # add data to map
      map <- leafem::addFeatures(
        map = map,
        data = self$site_geometry,
        groupId = group,
        color = pal(vals),
        fillColor = pal(vals),
        popup = leafpop::popupTable(
          x = popups,
          row.numbers = FALSE,
          feature.id = FALSE
        )
      )

      # add legend to map
      map <- leaflet::addLegend(
        layerId = "legend",
        map = map,
        pal = pal,
        values = vals,
        position = "bottomright",
      )

      # return map
      map
    },

    #' @description
    #' Render site data.
    #' @param height `character`/`numeric` CSS measurement value.
    #'  Default to `"100%"`.
    #' @param width `character`/`numeric` CSS measurement value.
    #'  Default to `"100%"`.
    #' @return [rhandsontable::rhandsontable] object.
    render_site_data = function(height = "100%", width = "100%") {
      # convert status column to factor
      d <- self$site_data
      d[[self$site_status_header]] <- factor(
        x = d[[self$site_status_header]],
        levels = self$get_action_ids()
      )
      # initialize table
      r <- rhandsontable::rhandsontable(
        d,
        height = height,
        width = width,
        useTypes = TRUE
      )
      r <- rhandsontable::hot_col(
        hot = r,
        col = c(1, 2, 3),
        readOnly = TRUE
      )
      r <- rhandsontable::hot_validate_character(
        hot = r,
        col = 4,
        choices = self$get_action_ids(),
        allowInvalid = FALSE
      )
      r <- rhandsontable::hot_validate_numeric(
        hot = r,
        cols = seq(5, ncol(d)),
        min = 0,
        max = 1e+6,
        allowInvalid = FALSE
      )
      # return table
      r
    },

    #' @description
    #' Render feature data.
    #' @param height `character`/`numeric` CSS measurement value.
    #'  Default to `"100%"`.
    #' @param width `character`/`numeric` CSS measurement value.
    #'  Default to `"100%"`.
    #' @return [rhandsontable::rhandsontable] object.
    render_feature_data = function(height = "100%", width = "100%") {
      # initialize table
      r <- rhandsontable::rhandsontable(
        self$feature_data,
        height = height,
        width = width,
        useTypes = TRUE
      )
      r <- rhandsontable::hot_col(r, col = 1, readOnly = TRUE)
      r <- rhandsontable::hot_validate_numeric(
        r, 2,
        min = 0, max = 100, allowInvalid = FALSE
      )
      r <- rhandsontable::hot_validate_numeric(
        r, 3,
        min = 0, max = 100, allowInvalid = FALSE
      )
      # return table
      r
    },

    #' @description
    #' Render feasibility data.
    #' @param height `character`/`numeric` CSS measurement value.
    #'  Default to `"100%"`.
    #' @param width `character`/`numeric` CSS measurement value.
    #'  Default to `"100%"`.
    #' @return [rhandsontable::rhandsontable] object.
    render_feasibility_data = function(height = "100%", width = "100%") {
      # initialize table
      r <- rhandsontable::rhandsontable(
        self$feasibility_data,
        height = height,
        width = width,
        useTypes = TRUE
      )
      r <- rhandsontable::hot_col(r, col = 1, readOnly = TRUE)
      r <- rhandsontable::hot_col(
        r,
        col = seq(2, ncol(self$feasibility_data)),
        type = "checkbox",
        renderer = paste0("
        function (instance, td, row, col, prop, value, cellProperties) {
           Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
           if (instance.params) {
             if (col > 0) {
               if (value > 0.5) {
                 td.style.background = '",
                 self$parameters$true_style$bgFill, "';
               } else {
                 td.style.background = '",
                 self$parameters$false_style$bgFill, "';
               }
             }
           }
        }")
      )
      # return table
      r
    },

    #' @description
    #' Render consequence data.
    #' @param action_id `character` identifier for action.
    #' @param height `character`/`numeric` CSS measurement value.
    #'  Default to `"100%"`.
    #' @param width `character`/`numeric` CSS measurement value.
    #'  Default to `"100%"`.
    #' @return [rhandsontable::rhandsontable] object.
    render_consequence_data = function(action_id,
                                       height = "100%",
                                       width = "100%") {
      # assert arguments are valid
      assertthat::assert_that(
        assertthat::is.string(action_id),
        assertthat::noNA(action_id),
        action_id %in% self$action_ids
      )
      # initialize table
      x <- self$consequence_data[[match(action_id, self$action_ids)]]
      r <- rhandsontable::rhandsontable(
        x,
        height = height,
        width = width,
        useTypes = TRUE
      )
      r <- rhandsontable::hot_col(r, col = 1, readOnly = TRUE)
      r <- rhandsontable::hot_validate_numeric(
        r, seq(2, ncol(x)),
        min = 0, max = 1e+6, allowInvalid = FALSE
      )
      # return table
      r
    },

    #' @description
    #' Write the data to disk.
    #' @param workbook_path `character` file path.
    #' @param geometry_path `character` file path.
    write = function(workbook_path, geometry_path) {
      # assert that arguments are valid
      assertthat::assert_that(
        assertthat::is.string(workbook_path),
        assertthat::noNA(workbook_path),
        assertthat::is.string(geometry_path),
        assertthat::noNA(geometry_path)
      )
      # workbook
      ## create workbook
      openxlsx::saveWorkbook(
        whatdataio::create_project_workbook(
          ## variables
          site_ids = self$site_ids,
          feature_ids = self$feature_ids,
          action_ids = self$action_ids,
          site_descriptions = self$site_descriptions,
          feature_descriptions = self$feature_descriptions,
          action_descriptions = self$action_descriptions,
          ## data
          site_data = self$site_data,
          feasibility_data = self$feasibility_data,
          feature_data = self$feature_data,
          consequence_data = self$consequence_data,
          ## parameters
          parameters = self$parameters
        ),
        file = workbook_path,
        overwrite = TRUE,
        returnValue = FALSE
      )
      # geometry data
      if (inherits(self$site_geometry, "sf")) {
        suppressWarnings({
          sf::write_sf(self$site_geometry, geometry_path)
        })
      }
      # return result
      invisible(self)
    }

  )
)

#' New project
#'
#' Create a new [Project] object.
#'
#'
#' This function creates an Excel Workbook with data and results.
#'
#' @param site_ids `character` identifiers for sites.
#'   No missing (`NA`) values are permitted. This object must contain at least
#'   one value.
#'
#' @param feature_ids `character` identifiers for biodiversity features.
#'   No missing (`NA`) values are permitted. This object must contain at least
#'   one value.
#'
#' @param action_ids `character` identifiers for management actions.
#'   No missing (`NA`) values are permitted. This object must contain at least
#'   one value.
#'
#' @param site_descriptions `character` descriptions of sites.
#'   No missing (`NA`) values are permitted. This object must contain at least
#'   one value.
#'
#' @param feature_descriptions `character` descriptions of biodiversity
#'   features.
#'   No missing (`NA`) values are permitted. This object must contain at least
#'   one value.
#'
#' @param action_descriptions `character` descriptions of management actions.
#'   No missing (`NA`) values are permitted. This object must contain at least
#'   one value.
#'
#' @param site_data `data.frame` containing site data.
#'
#' @param feasibility_data `data.frame`containing feasibility data.
#'
#' @param feature_data `data.frame` containing feature data.
#'
#' @param consequence_data `list` of `data.frame` objects
#'   containing consequence data.
#'
#' @param parameters `list` object containing parameters to customize
#'  appearance of worksheet.
#'
#' @param site_geometry [sf::st_sf()] object containing spatial boundaries
#'   for the sites.
#'   Defaults to `NULL` such that site locations are generated based
#'   on the longitude/latitude of each site (per argument to `site_data`).
#'
#' @param id `character` identifier for object.
#'
#' @return A [Project] object.
#'
#' @examples
#' #TODO
#' @export
new_project <- function(site_ids,
                        site_descriptions,
                        feature_ids,
                        feature_descriptions,
                        action_ids,
                        action_descriptions,
                        site_data,
                        feature_data,
                        feasibility_data,
                        consequence_data,
                        parameters,
                        site_geometry = NULL,
                        id = uuid::UUIDgenerate()) {
  # create geometry data if needed
  if (is.null(site_geometry)) {
    assertthat::assert_that(
      inherits(site_data, "data.frame"),
      ncol(site_data) >= 4,
      nrow(site_data) >= 1,
      is.numeric(site_data[[2]]),
      assertthat::noNA(site_data[[2]]),
      is.numeric(site_data[[3]]),
      assertthat::noNA(site_data[[3]]),
      is.character(site_ids),
      assertthat::noNA(site_ids),
      length(site_ids) == nrow(site_data)
    )
    site_geometry <- sf::st_as_sf(
      stats::setNames(site_data[, c(1, 2, 3), drop = FALSE], c("id", "x", "y")),
      coords = c("x", "y"),
      crs = 4326
    )
  }

  # reproject geometry data if needed
  if (sf::st_crs(site_geometry) != sf::st_crs(4326)) {
    site_geometry <- sf::st_transform(site_geometry, sf::st_crs(4326))
  }

  # create new dataset
  Project$new(
    id = id,
    site_ids = site_ids,
    site_descriptions = site_descriptions,
    site_geometry = site_geometry,
    feature_ids = feature_ids,
    feature_descriptions = feature_descriptions,
    action_ids = action_ids,
    action_descriptions = action_descriptions,
    site_data = site_data,
    feature_data = feature_data,
    feasibility_data = feasibility_data,
    consequence_data = consequence_data,
    parameters = parameters
  )
}
