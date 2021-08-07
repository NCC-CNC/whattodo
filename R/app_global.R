app_global <- quote({

  # set seed for reproducibility
  set.seed(200)

  # load parameters
  parameters <- RcppTOML::parseTOML(
    system.file("config.toml", package = "whattodo")
  )

})
