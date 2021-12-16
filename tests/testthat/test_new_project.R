context("new_project")

describe("new_project()", {
  # load data
  parameters <- whatdataio::read_data_configuration()
  d <- whatdataio::simulate_project_data(5, 3, 2, parameters)
  # create object
  p <- new_project(
    site_ids = d$site_ids,
    site_descriptions = d$site_descriptions,
    feature_ids = d$feature_ids,
    feature_descriptions = d$feature_descriptions,
    action_ids = d$action_ids,
    action_descriptions = d$action_descriptions,
    site_data = d$site_data,
    feature_data = d$feature_data,
    feasibility_data = d$feasibility_data,
    action_expectation_data = d$action_expectation_data,
    parameters = parameters,
    site_geometry = NULL
  )
  it("initializes", {
    expect_is(p, "Project")
    print(p)
    expect_equal(p$site_ids, d$site_ids)
    expect_equal(p$site_descriptions, d$site_descriptions)
    expect_equal(p$feature_ids, d$feature_ids)
    expect_equal(p$feature_descriptions, d$feature_descriptions)
    expect_equal(p$action_ids, d$action_ids)
    expect_equal(p$action_descriptions, d$action_descriptions)
    expect_equal(p$site_data, d$site_data)
    expect_equal(p$feature_data, d$feature_data)
    expect_equal(p$feasibility_data, d$feasibility_data)
    expect_equal(p$action_expectation_data, d$action_expectation_data)
    expect_equal(p$parameters, parameters)
    expect_is(p$site_geometry, "sf")
    expect_named(p$site_geometry, c("id", "geometry"))
    expect_equal(p$site_geometry$id, d$site_ids)
    expect_equal(nrow(p$site_geometry), length(d$site_ids))
  })
  it("has get methods", {
    expect_true(assertthat::is.string(p$repr()))
    expect_equal(p$get_site_ids(), d$site_ids)
    expect_equal(p$get_feature_ids(), d$feature_ids)
    expect_equal(p$get_action_ids(), d$action_ids)
    expect_equal(
      p$get_site_statuses(),
      d$site_data[["Current status"]]
    )
    expect_equal(
      p$get_action_costs(action_id = "action 1"),
      d$site_data[["Cost of “action 1”"]]
    )
    expect_equal(
      p$get_action_feasibility(action_id = "action 2"),
      d$feasibility_data[["Feasibility of “action 2”"]]
    )
    expect_equal(
      p$get_bbox(expand = FALSE),
      as.list(sf::st_bbox(p$site_geometry))[c("xmin", "xmax", "ymin", "ymax")]
    )
    expect_is(
      p$get_bbox(expand = TRUE),
      "list"
    )
    expect_is(
      p$get_max_feature_expectation(),
      "data.frame"
    )
    expect_is(
      p$get_current_feature_expectation(),
      "data.frame"
    )
  })
  it("has set methods", {
    ## set_feature_goal
    expect_true(p$get_feature_goal("feature 2") != 0.5)
    p$set_feature_goal("feature 2", 0.5)
    expect_equal(p$get_feature_goal("feature 2"), 0.5)
    ## set_feature_weight
    expect_true(p$get_feature_weight("feature 2") != 11)
    p$set_feature_weight("feature 2", 11)
    expect_equal(p$get_feature_weight("feature 2"), 11)
    ## set_site_data
    nd <- d$site_data[1, , drop = FALSE]
    p$set_site_data(nd)
    expect_equal(p$site_data, nd)
    p$set_site_data(d$site_data)
    ## set_feature_data
    nd <- d$feature_data[1, , drop = FALSE]
    p$set_feature_data(nd)
    expect_equal(p$feature_data, nd)
    p$set_feature_data(d$feature_data)
    ## set_feasibility_data
    nd <- d$feasibility_data[1, , drop = FALSE]
    p$set_feasibility_data(nd)
    expect_equal(p$feasibility_data, nd)
    p$set_feasibility_data(d$feasibility_data)
    ## set_action_expectation_data
    nd <- d$action_expectation_data[[2]][1, , drop = FALSE]
    p$set_action_expectation_data(nd, "action 2")
    expect_equal(p$action_expectation_data[[2]], nd)
    p$set_action_expectation_data(d$action_expectation_data[[2]], "action 2")
  })
  it("has widget data methods", {
    expect_is(p$get_solution_settings_data(), "list")
    expect_is(p$get_goals_settings_data(), "list")
    expect_is(p$get_weights_settings_data(), "list")
    expect_is(p$settings[[1]]$get_widget_data(), "list")
  })
  it ("has methods extracting optimization data", {
    expect_is(p$get_pu_data(), "tbl_df")
    expect_is(p$get_zone_data(), "ZonesCharacter")
    expect_is(p$get_goal_data(), "tbl_df")
    expect_is(p$get_weight_data(), "tbl_df")
    expect_is(p$get_locked_data(), "tbl_df")
  })
  it ("has map render methods", {
    expect_is(
      p$render_on_map(leaflet::leaflet(), data = "location"),
      "leaflet"
    )
    expect_is(
      p$render_on_map(leaflet::leaflet(), data = "status"),
      "leaflet"
    )
    expect_is(
      p$render_on_map(
        leaflet::leaflet(), data = "feasibility", action_id = "action 1"
      ),
      "leaflet"
    )
    expect_is(
      p$render_on_map(
        leaflet::leaflet(), data = "cost", action_id = "action 1"
      ),
      "leaflet"
    )

  })
  it ("has data render methods", {
    expect_is(p$render_site_data(), "rhandsontable")
    expect_is(p$render_feature_data(), "rhandsontable")
    expect_is(p$render_feasibility_data(), "rhandsontable")
    expect_is(
      p$render_action_expectation_data(action_id = "action 1"),
      "rhandsontable"
    )
  })
  it ("has write method", {
    # create temp file paths
    tmp_dir <- tempfile()
    dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
    f1 <- tempfile(tmpdir = tmp_dir, fileext = ".xlsx")
    f2 <- tempfile(tmpdir = tmp_dir, fileext = ".shp")
    # write data
    p$write(f1, f2)
    # tests
    expect_true(file.exists(f1))
    expect_true(file.exists(f2))
    # clean up
    unlink(tmp_dir, force = TRUE, recursive = TRUE)
  })
})
