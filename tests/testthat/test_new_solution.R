context("new_solution")

describe("new_solution()", {
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
    consequence_data = d$consequence_data,
    parameters = parameters,
    site_geometry = NULL
  )
  s <- prioritization_with_budget(
    site_ids = p$site_ids,
    feature_ids = p$feature_ids,
    action_ids = p$action_ids,
    pu_data = p$get_pu_data(),
    zone_data = p$get_zone_data(),
    goal_data = p$get_goal_data(),
    weight_data = p$get_weight_data(),
    locked_data = p$get_locked_data(),
    budget = 1500,
    gap = 0,
    parameters = parameters,
    verbose = FALSE
  )
  x <- new_solution(
    project = p,
    name = "sol",
    settings = p$settings,
    summary_results = s$summary_results,
    site_results = s$site_results,
    feature_results = s$feature_results
  )
  it("initializes", {
    expect_is(x, "Solution")
    print(x)
    expect_true(assertthat::is.string(x$repr()))
    expect_equal(x$project, p)
    expect_equal(x$summary_results, s$summary_results)
    expect_equal(x$site_results, s$site_results)
    expect_equal(x$feature_results, s$feature_results)
  })
  it("has get methods", {
    expect_equal(x$get_site_ids(), p$get_site_ids())
    expect_equal(x$get_feature_ids(), p$get_feature_ids())
    expect_equal(x$get_action_ids(), p$get_action_ids())
    expect_equal(x$get_bbox(), p$get_bbox())
    expect_is(p$get_map_layers(), "character")
  })
  it("has widget data methods", {
    expect_is(
      x$get_solution_results_data(),
      "list"
    )
  })
  it ("has map render methods", {
    l <- leaflet::leaflet()
    for (i in x$get_map_layers()) {
      expect_is(x$render_on_map(l, i), "leaflet")
    }
  })
  it ("has data render methods", {
    expect_is(x$render_site_data(), "rhandsontable")
    expect_is(x$render_feature_data(), "rhandsontable")
    expect_is(x$render_feasibility_data(), "rhandsontable")
    expect_is(x$render_consequence_data("action 1"), "rhandsontable")
    expect_is(x$render_site_results(), "datatables")
    expect_is(x$render_summary_results(), "datatables")
    expect_is(x$render_feature_results(), "datatables")
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
