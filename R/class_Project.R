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

    #' @field action_expectation_data `list` of `data.frame` objects.
    action_expectation_data = NULL,

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

    #' @field action_expectation_action_headers `character` named vector.
    action_expectation_action_headers = NA_character_,

    #' @field action_expectation_feature_headers `character` named vector.
    action_expectation_feature_headers = NA_character_,


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
    #' @param action_expectation_data `list` of `data.frame` objects.
    #' @param parameters `list` of parameters.
    #' @return A new Project object.
    initialize = function(id,
                          site_ids, site_descriptions, site_geometry,
                          feature_ids, feature_descriptions,
                          action_ids, action_descriptions,
                          site_data,
                          feature_data,
                          feasibility_data,
                          action_expectation_data,
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
        inherits(action_expectation_data, "list"),
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
      self$feature_descriptions <- feature_descriptions
      self$action_ids <- action_ids
      self$action_descriptions <- action_descriptions
      self$site_data <- site_data
      self$feasibility_data <- feasibility_data
      self$action_expectation_data <- action_expectation_data
      self$parameters <- parameters
      self$site_geometry <- site_geometry

      # assign fields derived from parameters
      ## site data headers
      self$site_cost_headers <- setNames(
        glue::glue(
          parameters$site_data_sheet$action_cost_header,
          action_ids = action_ids
        ),
        action_ids
      )
      self$site_status_header <- parameters$site_data_sheet$status_header
      ## action data headers
      self$action_feasibility_headers <- setNames(
        glue::glue(
          parameters$feasibility_data_sheet$action_feasibility_header,
          action_ids = action_ids
        ),
        action_ids
      )
      ## feature data headers
      self$feature_goal_header <- parameters$feature_data_sheet$target_header
      self$feature_weight_header <- parameters$feature_data_sheet$weight_header
      ## action expectation data headers
      self$action_expectation_feature_headers <- setNames(
        glue::glue(
          parameters$action_expectation_sheet$action_expectation_header,
          feature_ids = feature_ids
        ),
        feature_ids
      )
      self$action_expectation_action_headers <- setNames(
        glue::glue(
          parameters$action_expectation_sheet$sheet_name,
          action_ids = action_ids
        ),
        action_ids
      )

      # assign additional fields
      self$action_colors <- default_colors(action_ids)

      # add field for total budget setting
      ## calculate maximum total cost
      max_cost <- site_data[, unname(self$site_cost_headers), drop = FALSE]
      max_cost <- sum(apply(as.matrix(max_cost), 1, max, na.rm = TRUE))
      ## add setting to control budget
      self$settings <- list(
        new_parameter(
          name = "Total budget",
          status = FALSE,
          value = 1,
          min_value = 1,
          max_value = 100,
          step_value = 1,
          units = "%",
          reference_value = max_cost,
          reference_units = "CAD",
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
    #' Get action identifiers.
    #' @return `character` vector.
    get_action_ids = function() {
      self$action_ids
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
    #' Get action expectation data for features.
    #' @param action_id `character` identifier for action.
    #' @param feature_id `character` identifier for feature.
    #' @return `numeric` vector.
    get_action_expectations_for_feature = function(action_id, feature_id) {
      assertthat::assert_that(
        assertthat::is.string(action_id),
        assertthat::noNA(action_id),
        action_id %in% self$action_ids,
        assertthat::is.string(feature_id),
        assertthat::noNA(feature_id),
        action_id %in% self$feature_ids
      )
      nm1 <- self$action_expectation_action_headers[[action_id]]
      nm2 <- self$action_expectation_feature_headers[[feature_id]]
      self$action_expectation_data[[nm1]][[nm2]]
    },

    #' @description
    #' Get the highest amount for each feature.
    get_action_expectation_feature_maxima = function() {
      tibble::tibble(
        feature_id = self$feature_ids,
        amount = vapply(
          seq_along(self$feature_ids), FUN.VALUE = numeric(1), function(i) {
            ii <- self$action_expectation_feature_headers[[i]]
            m <- vapply(
              seq_along(self$action_ids),
              FUN.VALUE = numeric(length(self$site_ids)),
              function(j) action_expectation_data[[j]][[ii]]
            )
            sum(apply(m, 1, max, na.rm = TRUE))
          }
        )
      )
    },

    #' @description
    #' Get planning unit data for optimization.
    get_pu_data = function() {
      ## extract cost
      cost_data <- self$site_data[, self$site_cost_headers, drop = FALSE]
      ## extract action expectation data
      expectation_data <- lapply(seq_along(self$action_ids), function(i) {
        out <- action_expectation_data[[i]]
        out <- out[, self$action_expectation_feature_headers, drop = FALSE]
        names(out) <- paste0(self$action_ids[i], "_", self$feature_ids)
      })
      ## return result
      do.call(
        dplyr::bind_cols,
        append(
          list(tibble::tibble(site = selfsite_ids), cost_data),
          expectation_data
        )
      )
    },

    #' @description
    #' Get zone data for optimization.
    get_zone_data = function() {
      ## create zone name column
      args <- lapply(
        self$action_ids, function(x) paste0(x, "_", self$feature_ids)
      )
      ## append feature and zone names to object
      args <- append(
        args,
        list(zone_names = self$action_ids, feature_names = feature_ids)
      )
      ## return zones object
      do.call(prioritizr::zones, args)
    },


    #' @description
    #' Get goal data for optimization.
    get_goal_data = function() {
      tibble::tibble(
        feature = self$feature_ids,
        zone = list(self$action_ids)[rep(1, length(self$feature_ids))],
        type = "absolute",
        sense = ">=",
        target = c(
          feature_data[[self$feature_goal_header]] *
          self$get_action_expectation_feature_maxima()$amount
        )
      )
    },

    #' @description
    #' Get weight data for optimization.
    get_weight_data = function() {
      tibble::tibble(
        feature = feature_ids,
        weight = feature_data[[self$feature_weight_header]]
      )
    },

    #' @description
    #' Get locked data for optimization.
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
    #' @param native `logical` indicating if the bounding box should
    #'   be in (`TRUE`) the native coordinate reference system or (`FALSE`)
    #'   re-projected to longitude/latitude?
    #' @param expand `FALSE` should the bounding box be expanded by 10%?
    #' @return `list` object with `"xmin"`, `"xmax"`, `"ymin"`, and `"ymax"`
    #'   elements.
    get_bbox = function(native = TRUE, expand = FALSE) {
      # assert arguments are valid
      assertthat::assert_that(
        assertthat::is.flag(native),
        assertthat::noNA(native),
        assertthat::is.flag(expand),
        assertthat::noNA(expand)
      )
      # generate extent object
      if (native) {
        # if native then extract extent
        ext <- as.list(sf::st_bbox(self$site_geometry))
      } else {
        # if not native, then reproject data and extract extent
        ext <- sf::st_as_sf(
          sf::st_bbox(self$site_geometry), crs = sf::st_crs(self$site_geometry)
        )
        ext <- as.list(sf::st_bbox(sf::st_transform(ext, 4326)))
      }
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
      if (!native) {
        out$xmin <- max(out$xmin, -180)
        out$xmax <- min(out$xmax, 180)
        out$ymin <- max(out$ymin, -90)
        out$ymax <- min(out$ymax, 90)
      }
      # return result
      out
    },

    #' @description
    #' Add to map.
    #' @param map [leaflet::leaflet()] object.
    #' @param data `character` name of dataset to show.
    #' This should be equal to one of: `"location"`,
    #' `"status"`,
    #' one of the action identifiers with a "cost_" prefix,
    #' one of the action identifiers with a "feasibility_" prefix,
    #' or one of the action and feature identifiers with a "action_expectation_"
    #' prefix.
    #' For example, to show the cost of implementing action `"none"`,
    #' then the argument to data should be `"cost_none"`
    #' Additionally, to show the action expectation for the action
    #' `"none"` and the feature `"SPP1"`, then the argument should be
    #' `"action_expectation_data_none&&&&&&&&&&&&SPP2"`.
    #' Defaults to `"location"` such that the location of sites is shown
    #' by displaying all sites with the same color.
    #' @param group `character` group name. Defaults to `"sites"`.
    #' @return [leaflet::leaflet()] map.
    add_to_map = function(map, data = "location", group = "sites") {
      # assert that arguments are valid
      assertthat::assert_that(
        inherits(map, "leaflet"),
        assertthat::is.string(data),
        assertthat::noNA(data),
        assertthat::is.string(group),
        assertthat::noNA(group)
      )

      # prepare data for map
      ## display location
      if (identical(data, "location")) {
        pal <- leaflet::colorFactor(
          palette = default_colors("id")[[1]],
          domain = "Sites",
        )
        vals <- rep("Sites", nrow(self$geometry_data))
        popups <- setNames(
          object = tibble::tibble(name = site_ids),
          nm = self$parameters$site_data_sheet$name_header
        )
      ## display status
      } else if (identical(data, "status")) {
        pal <- leaflet::colorFactor(
          palette = self$action_colors,
          domain = names(self$action_ids),
        )
        popups <- setNames(
          object = tibble::tibble(
            name = site_ids,
            action = self$site_data[[4]]
          ),
          nm = self$parameters$site_data_sheet$status_header
        )
        vals <- popups[[2]]
      ## display cost
      } else if (startsWith(data, "cost_")) {
        # extract action if possible
        curr_action <- gsub("cost_", "", data, fixed = TRUE)
        pal <- leaflet::colorNumeric(
          palette = "inferno",
          domain = range(self$get_action_costs(curr_action))
        )
        popups <- setNames(
          object = tibble::tibble(
            name = site_ids,
            cost = self$get_action_costs(curr_action)
          ),
          nm = c(
            self$parameters$site_data_sheet$name_header,
            glue::glue(
              self$parameters$site_data_sheet$action_cost_header,
              action_ids = curr_action
            )
          )
        )
        vals <- popups[[2]]
      ## display feasibility
      } else if (startsWith(data, "feasibility_")) {
        curr_action <- gsub("feasibility_", "", data, fixed = TRUE)
        pal <- leaflet::colorFactor(
          palette = c(
            self$parameters$true_style$bgFill,
            self$parameters$false_style$bgFill
          ),
          domain = c("feasible", "infeasible"),
        )
        popups <- setNames(
          object = tibble::tibble(
            name = site_ids,
            cost = self$get_action_costs(curr_action)
          ),
          nm = c(
            self$parameters$site_data_sheet$name_header,
            glue::glue(
              self$parameters$feasibility_data_sheet$action_feasibility_header,
              action_ids = curr_action
            )
          )
        )
        vals <- popups[[2]]
      ## display action expectation data
      } else if (startsWith(data, "action_expectation_")) {
        curr_action <- gsub("action_expectation_", "", data, fixed = TRUE)
        curr_action <- strsplit(curr_action, "&&&&&&&&&&&&")
        asserrthat::assert_that(
          length(curr_action) == 2,
          msg = "invalid format for action expectation data"
        )
        curr_action <- curr_action[[1]]
        curr_feature <- curr_feature[[2]]
        pal <- leaflet::colorNumeric(
          palette = "viridis",
          domain = range(
            self$get_action_expectations(curr_action, curr_feature)
          )
        )
        popups <- setNames(
          object = tibble::tibble(
            name = site_ids,
            value = self$get_action_expectations(curr_action, curr_feature)
          ),
          nm = c(
            self$parameters$site_data_sheet$name_header,
            glue::glue(
              self$parameters$feasibility_data_sheet$action_feasibility_header,
              action_ids = curr_action
            )
          )
        )
        vals <- popups[[2]]
      } else {
        stop("invalid argument to data")
      }

      # clear group from map
      map <- leaflet::clearGroup(map, group)

      # add data to map
      map <- leafem::addFeatures(
        map = map,
        groupId = group,
        color = pal(vals),
        fillColor = pal(vals),
        popup = site_popups
      )

      # add legend to map
      map <- leaflet::addLegend(
        pal = pal,
        values = vals,
        group = group,
        position = "bottomright",
      )

      # return map
      map
    },

    #' @description
    #' Get data for rendering the solution settings widget.
    get_solution_settings_data = function() {
      list(
        themes = self$get_goals_settings_data(),
        weights = self$get_weights_settings_data(),
        parameters = lapply(self$settings, x$get_widget_data())
      )
    },

    #' @description
    #' Get data for rendering widget to specify goals.
    get_goals_settings_data = function() {
      # extract column names from parameters
      nh <- self$parameters$feature_data_sheet$name_header
      th <- self$parameters$feature_data_sheet$target_header
      # generate data
      lapply(seq_len(nrow(self$feature_data)), function(x) {
        list(
          id = paste0("T", convert_to_id(self$feature_data[[nh]][[i]])),
          name = self$feature_data[[nh]][[i]],
          feature_name = self$feature_data[[nh]][[i]],
          feature_id = paste0("F", convert_to_id(self$feature_data[[nh]][[i]])),
          feature_status = isTRUE(self$feature_data[[th]][[i]] > 1e-10),
          feature_total_amount = self$current_data$total[[i]],
          feature_current_held = self$current_data$held[[i]],
          feature_min_goal = 0,
          feature_max_goal = 1,
          feature_goal = self$feature_data[[th]][[i]],
          feature_limit_goal = 0,
          feature_step_goal = 0.01,
          units = "units"
        )
      })
    },

    #' @description
    #' Get data for rendering widget to specify weights.
    get_weights_settings_data = function() {
      # extract column names from parameters
      nh <- self$parameters$feature_data_sheet$name_header
      wh <- self$parameters$feature_data_sheet$weight_header
      # generate data
      lapply(seq_len(nrow(self$feature_data)), function(x) {
        list(
          id = paste0("W", convert_to_id(self$feature_data[[nh]][[i]])),
          name = self$feature_data[[nh]][[i]],
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
        assertthat::is.number(value),
        assertthat::noNA(value)
      )
      i <- which(self$feature_ids == feature_id)
      self$feature_data[[self$feature_goal_header]][[i]] <- value
    },

    #' @description
    #' Set weight for a feature.
    #' @param feature_id `character` value containing the feature identifier.
    #' @param value `numeric` value containing the new weight.
    set_feature_weight = function(feature_id, value) {
      assertthat::assert_that(
        assertthat::is.string(feature_id),
        assertthat::noNA(feature_id),
        assertthat::is.number(value),
        assertthat::noNA(value)
      )
      i <- which(self$feature_ids == feature_id)
      self$feature_data[[self$feature_weight_header]][[i]] <- value
    },

    #' @description
    #' Set site data.
    #' @param x `data.frame` containing new data.
    set_site_data = function(x) {
      assertthat::assert_that(
        inherits(x, "data.frame"),
        identical(names(x), self$site_data)
      )
      self$site_data <- tibble::as_tibble(x)
    },

    #' @description
    #' Set feature data.
    #' @param x `data.frame` containing new data.
    set_feature_data = function(x) {
      assertthat::assert_that(
        inherits(x, "data.frame"),
        identical(names(x), self$feature_data)
      )
      self$feature_data <- tibble::as_tibble(x)
    },

    #' @description
    #' Set feasibility data.
    #' @param x `data.frame` containing new data.
    set_feasibility_data = function(x) {
      assertthat::assert_that(
        inherits(x, "data.frame"),
        identical(names(x), self$feasibility_data)
      )
      self$feasibility_data <- tibble::as_tibble(x)
    },

    #' @description
    #' Set action expectation data.
    #' @param x `data.frame` containing new data.
    #' @param action_id `character` identifier for action.
    set_action_expectation_data = function(x, action_id) {
      assertthat::assert_that(
        assertthat::is.string(action_id),
        assertthat::noNA(action_id),
        action_id %in% self$action_ids
      )
      i <- self$action_expectation_action_headers[[action_id]]
      assertthat::assert_that(
        inherits(x, "data.frame"),
        identical(names(x), self$action_expectation_data[[i]])
      )
      self$action_expectation_data[[i]] <- tibble::as_tibble(x)
    },

    #' @description
    #' Render site data using \pkg{rhandsontable}.
    render_site_data = function() {
      # initialize table
      r <- rhandsontable::rhandsontable(self$site_data, useTypes = TRUE)
      r <- rhandsontable::hot_col(r, col = c(1, 2, 3), readOnly = TRUE)
      r <- rhandsontable::hot_validate_character(
        r, col = 4, choices = project$get_action_ids(), allowInvalid = FALSE
      )
      r <- rhandsontable::hot_validate_numeric(
        r, seq(5, ncol(x)),
        min = 0, max = 1e+6, allowInvalid = FALSE
      )
      # return table
      r
    },

    #' @description
    #' Render feature data using \pkg{rhandsontable}.
    render_feature_data = function() {
      # initialize table
      r <- rhandsontable::rhandsontable(self$feature_data, useTypes = TRUE)
      r <- rhandsontable::hot_col(r, col = 1, readOnly = TRUE)
      r <- rhandsontable::hot_validate_numeric(
        r, 2,
        min = 0, max = 1e+6, allowInvalid = FALSE
      )
      r <- rhandsontable::hot_validate_numeric(
        r, 3,
        min = 0, max = 100, allowInvalid = FALSE
      )
      # return table
      r
    },

    #' @description
    #' Render feasibility data using \pkg{rhandsontable}.
    render_feasibility_data = function() {
      # convert binary values to logical
      for (i in seq(2, ncol(x))) {
        x[[i]] <- as.logical(x[[i]])
      }
      # initialize table
      r <- rhandsontable::rhandsontable(
        self$feasibility_data, useTypes = TRUE
      )
      r <- rhandsontable::hot_col(r, col = 1, readOnly = TRUE)
      r <- rhandsontable::hot_col(
        r,
        col = seq(2, ncol(x)), type = "checkbox", renderer = paste0("
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
    #' Render action expectation data using \pkg{rhandsontable}.
    #' @param action_id `character` identifier for action.
    render_action_expectation_data = function(action_id) {
      # assert arguments are valid
      assertthat::assert_that(
        assertthat::is.string(action_id),
        assertthat::noNA(action_id),
        action_id %in% self$action_ids
      )
      # initialize table
      r <- rhandsontable::rhandsontable(x, useTypes = TRUE)
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
          action_expectation_data = self$action_expectation_data,
          ## comments
          site_comments = template_site_comments(
            site_descriptions = self$site_descriptions,
            action_descriptions = self$action_descriptions,
            parameters = self$parameters
          ),
          feasibility_comments = template_feasibility_comments(
            site_descriptions = self$site_descriptions,
            action_descriptions = self$action_descriptions,
            parameters = self$parameters
          ),
          feature_comments = template_feature_comments(
            feature_descriptions = self$feature_descriptions,
            parameters = self$parameters
          ),
          action_expectation_comments = lapply(
            self$action_ids, function(i) {
              whatdataio::template_action_expectation_comments(
                site_descriptions = self$site_descriptions,
                feature_descriptions = self$feature_descriptions,
                action_id = i,
                parameters = self$parameters
              )
            }
          ),
          ## parameters
          parameters = parameters
        ),
        returnValue = FALSE
      )
      # geometry data
      if (inherits(self$geometry_data, "sf")) {
        suppressWarnings({
          sf::write_sf(self$geometry_data, spatial_path)
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
#' @param action_expectation_data `list` of `data.frame` objects
#'   containing expectation data.
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
                        action_expectation_data,
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
      assertthat::assert_that(site_data[[2]]),
      is.numeric(site_data[[3]]),
      assertthat::assert_that(site_data[[3]]),
      is.character(site_ids),
      assertthat::noNA(site_ids),
      length(site_ids) == nrow(site_data)
    )
    site_geometry <- sf::st_as_sf(
      id = site_ids,
      coords = c("x", "y"),
      setNames(site_data[, c(2, 3), drop = FALSE], c("x", "y")),
      crs = 4326
    )
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
    action_expectation_data = action_expectation_data,
    parameters = parameters
  )
}
