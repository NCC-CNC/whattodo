#' Sever function: generate new solution
#'
#' Set behavior for generating new solutions.
#'
#' @param input,output,session Arguments inherited from [shiny::shinyServer].
#'
#' @details
#' This object is designed to be used within [app_server] function.
#' Within the [app_server] function, it should be called like this:
#'
#' ```
#' eval(server_generate_new_solution)
#' ```
#'
#' @noRd
server_generate_new_solution <- quote({

  # create reactive value to store new results
  new_user_result <- shiny::reactiveVal()

  # stop processing when stop button solution pressed
  shiny::observeEvent(input$newSolutionPane_settings_stop_button, {
    ## specify dependencies
    shiny::req(input$newSolutionPane_settings_stop_button)
    shiny::req(app_data$new_solution_id)

    ## stop processing if possible given strategy
    if (identical(strategy, "multicore")) {
      ## kill task if possible
      suppressWarnings(ipc::stopMulticoreFuture(app_data$task))

      ## reset app state
      app_data$new_solution_id <- NULL

      ## reset buttons and input widgets
      shinyFeedback::resetLoadingButton("newSolutionPane_settings_start_button")
      enable_html_element("newSolutionPane_settings_name")
      shinyjs::disable("newSolutionPane_settings_stop_button")
    }
  })

  # generate new solution when start button pressed
  shiny::observeEvent(input$newSolutionPane_settings_start_button, {
    ## specify dependencies
    shiny::req(input$newSolutionPane_settings_start_button)
    shiny::req(input$newSolutionPane_settings_name)

    ## update generate solution inputs
    disable_html_element("newSolutionPane_settings_start_button")
    disable_html_element("newSolutionPane_settings_name")

    ## generate id and store it in app_data
    curr_id <- uuid::UUIDgenerate()
    app_data$new_solution_id <- curr_id
    app_data$new_solution_data <- app_data$project$clone(deep = TRUE)

    ## extract values for generating result
    ### settings
    curr_budget <- app_data$project$settings[[1]]$get_value()
    curr_type <- app_data$project$settings[[1]]$status
    curr_name <- input$newSolutionPane_settings_name

    ### ids
    curr_site_ids <- app_data$project$site_ids
    curr_feature_ids <- app_data$project$feature_ids
    curr_action_ids <- app_data$project$action_ids
    ### data
    curr_pu_data <- app_data$project$get_pu_data()
    curr_zone_data <- app_data$project$get_zone_data()
    curr_goal_data <- app_data$project$get_goal_data()
    curr_weight_data <- app_data$project$get_weight_data()
    curr_locked_data <- app_data$project$get_locked_data()
    ### parameters
    curr_parameters <- app_data$project$parameters
    curr_time_limit <- get_golem_config("solver_time_limit")
    curr_gap <- get_golem_config("solver_gap")
    curr_verbose <- get_golem_config("verbose")

    ## enable stop button
    shinyjs::enable("newSolutionPane_settings_stop_button")

    ## generate result using asynchronous task
    app_data$task <- future::future(packages = "whattodo", seed = NULL, {
      ### main processing
      if (curr_type) {
        #### if budget specified, then use the min shortfall formulation
        r <- try(
          prioritization_with_budget(
            site_ids = curr_site_ids,
            feature_ids = curr_feature_ids,
            action_ids = curr_action_ids,
            pu_data = curr_pu_data,
            zone_data = curr_zone_data,
            goal_data = curr_goal_data,
            weight_data = curr_weight_data,
            locked_data = curr_locked_data,
            budget = curr_budget,
            parameters = curr_parameters,
            verbose = curr_verbose,
            gap = curr_gap,
            time_limit = curr_time_limit
          ),
          silent = TRUE
        )
      } else {
        #### else, then use the min set formulation
        r <- try(
          prioritization_without_budget(
            site_ids = curr_site_ids,
            feature_ids = curr_feature_ids,
            action_ids = curr_action_ids,
            pu_data = curr_pu_data,
            zone_data = curr_zone_data,
            goal_data = curr_goal_data,
            locked_data = curr_locked_data,
            parameters = curr_parameters,
            verbose = curr_verbose,
            gap = curr_gap,
            time_limit = curr_time_limit
          ),
          silent = TRUE
        )
      }
      ## return result
      list(id = curr_id, name = curr_name, type = curr_type, result = r)
    })

    ## add promises to handle result once asynchronous task finished
    prom <-
      (app_data$task) %...>%
      (function(result) {
        new_user_result(result)
      }) %...!%
      (function(error) {
        new_user_result(NULL)
        if (!is.null(app_data$new_solution_id)) {
          warning(error)
        }
        NULL
      })

      ## this needed to implement asynchronous processing,
      ## see https://github.com/rstudio/promises/issues/23
      NULL
    }
  )

  # add solution to map when generating new solution
  shiny::observeEvent(new_user_result(), {
    ## specify dependencies
    if (is.null(new_user_result()) || is.null(app_data$new_solution_id)) {
      return()
    }
    if (!identical(new_user_result()$id, app_data$new_solution_id)) {
      return()
    }

    ## extract result
    r <- new_user_result()

    ## if failed to generate solution...
    if (inherits(r$result, "try-error")) {
      ### throw warning in development mode
      if (golem::app_dev()) {
        if (requireNamespace("whereami", quietly = TRUE)) {
          whereami::whereami()
        }
        cli::cli_verbatim(r$result)
        cli::rule()
      }
      ### display modal
      shinyalert::shinyalert(
        title = "Oops",
        text = shiny::includeMarkdown(
          ifelse(
            r$type,
            system.file(
              "app", "text", "error_with_budget.md", package = "whattodo"
            ),
            system.file(
              "app", "text", "error_without_budget.md", package = "whattodo"
            )
          )
        ),
        size = "s",
        html = TRUE,
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        type = "error",
        showConfirmButton = TRUE,
        confirmButtonText = "OK",
        timer = 0,
        confirmButtonCol = "#0275d8",
        animation = TRUE
      )
      ### reset button
      shinyFeedback::resetLoadingButton("newSolutionPane_settings_start_button")
      enable_html_element("newSolutionPane_settings_name")
      ## exit
      return()
    }

    ## create budget parameter to store settings
    curr_budget_settings <- app_data$project$settings[[1]]$clone(deep = TRUE)
    curr_budget_settings$status <- is.na(r$budget)
    curr_budget_settings$value <- ifelse(is.na(r$budget), 0, r$budget)

    ## generate solution from result
    s <- new_solution(
      id = uuid::UUIDgenerate(),
      name = r$name,
      project = app_data$new_solution_data,
      settings = list(curr_budget_settings),
      summary_results = r$result$summary_results,
      site_results = r$result$site_results,
      feature_results = r$result$feature_results
    )

    ## store solution
    app_data$solution <- append(
      app_data$solution,
      stats::setNames(list(s), s$id)
    )

    ## store solution id and names
    app_data$solution_ids <- c(
      app_data$solution_ids,
      stats::setNames(s$id, s$name)
    )

    ## add new solution to export sidebar
    shiny::updateSelectInput(
      session = session,
      inputId = "exportPane_select",
      choices = c(
        stats::setNames(app_data$project_data_id, "Project data"),
        app_data$solution_ids
      )
    )

    ### add new solution to solution results widget
    addSolutionResults(
      session = session,
      inputId = "solutionResultsPane_results",
      value = s
    )

    ## show solution in sidebar
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "solutionResultsPane_results_select",
      choices = app_data$solution_ids,
      selected = dplyr::last(app_data$solution_ids)
    )
    showSolutionResults(
      session = session,
      inputId = "solutionResultsPane_results",
      value = s$id
    )

    ## add new solution to solution results modal
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "solutionModal_select",
      choices = app_data$solution_ids,
      selected = dplyr::last(app_data$solution_ids)
    )

    ## show solution results sidebar
    leaflet.extras2::openSidebar(
      map = leaflet::leafletProxy("map"),
      id = "solutionResultsPane",
      sidebar_id = "mainSidebar"
    )

    ## show solution on map
    shiny::updateSelectInput(
      session = session,
      inputId = "map_dataset",
      choices = c(
        stats::setNames(app_data$project_data_id, "Project data"),
        app_data$solution_ids
      ),
      select = dplyr::last(app_data$solution_ids)
    )
    shiny::updateSelectInput(
      session = session,
      inputId = "map_layer",
      choices = dplyr::last(app_data$solution)$get_map_layers(),
      select = "priority_actions"
    )

    ## reset solution name
    shiny::updateTextInput(
      session = session,
      inputId = "newSolutionPane_settings_name",
      value = ""
    )

    ### enable solution results modal button after generating first solution
    if (length(app_data$solution) == 1) {
      enable_html_css_selector("#mainSidebar li:nth-child(2)")
    }

    ## reset buttons and input widgets
    disable_html_element("newSolutionPane_settings_stop_button")
    shinyFeedback::resetLoadingButton("newSolutionPane_settings_start_button")
    disable_html_element("newSolutionPane_settings_start_button")
    enable_html_element("newSolutionPane_settings_name")
  })

})
