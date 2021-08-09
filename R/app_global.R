app_global <- quote({

  # set seed for reproducibility
  set.seed(200)

  # load parameters for data handling
  parameters <- whatdataio::read_data_configuration()

  # append color variables to parameters
  parameters$map <- list(
    basemap_name = whattodo::get_golem_config("basemap_name"),
    basemap_key = whattodo::get_golem_config("basemap_key"),
    defaultColorName = whattodo::get_golem_config("default_color_name"),
    defaultColorCode = whattodo::get_golem_config("default_color_code"),
    actionColorNames = whattodo::get_golem_config("action_color_names"),
    actionColorCodes = whattodo::get_golem_config("action_color_codes")
  )

})
