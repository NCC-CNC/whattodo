app_global <- quote({

  # set seed for reproducibility
  set.seed(200)

  # load parameters for data handling
  parameters <- whatdataio::read_data_configuration()

  # print initial memory usage
  if (isTRUE(whattodo::get_golem_config("monitor"))) {
      cli::cli_rule()
      golem::print_dev("Initial memory used: ")
      golem::print_dev(pryr::mem_used())
  }

  # find built-in projects
  # if environmental variable "FORCE_DEFAULT_PROJECTS=true":
  #   then use built-in projects distributed with shiny app
  #
  # elif "projects: default" in golem-config.yml:
  #   then use built-in projects distributed with shiny app
  #
  # elif environmental variable "SHINYPROXY_USERGROUPS=public"
  #   then use projects only public projects available at the
  #   location "projects" location in golem config
  #
  # else:
  #   then import projects from location specified in golem-config.yml

  # set user group
  user_groups <- Sys.getenv("SHINYPROXY_USERGROUPS")
  user_groups <- tolower(gsub(" ", "", user_groups, fixed = TRUE))
  if (nchar(user_groups) == 0) {
    user_groups <- "public"
  }
  user_groups <- strsplit(user_groups, ",", fixed = TRUE)[[1]]

  # ensure that public projects are always available
  user_groups <- unique(c("public", user_groups))

  # set project data directory
  if (identical(Sys.getenv("FORCE_DEFAULT_PROJECTS"), "true")) {
    project_dir <- system.file("extdata", "projects", package = "whattodo")
  } else if (identical(whattodo::get_golem_config("projects"), "default")) {
    project_dir <- system.file("extdata", "projects", package = "whattodo")
  } else {
    project_dir <- whattodo::get_golem_config("projects")
  }

  # import projects
  project_data <- whattodo::find_projects(project_dir, user_groups)

})
