context("ui")

test_that("leaflet_map", {
  x <- leaflet_map("id")
  expect_is(x, "leaflet")
})

test_that("dataModal", {
  x <- dataModal(
    id = "id",
    trigger = "trig"
  )
  expect_is(x, "shiny.tag")
})

test_that("helpModal", {
  x <- helpModal("id", "trigger_id")
  expect_is(x, "shiny.tag")
})

test_that("errorModal", {
  x <- errorModal("id", "trigger_id")
  expect_is(x, "shiny.tag")
})

test_that("importModal", {
  x <- importModal("id")
  expect_is(x, "shiny.tag")
})

test_that("exportSidebarPane", {
  x <- exportSidebarPane("id")
  expect_is(x, "shiny.tag")
})

test_that("acknowledgmentsSidebarPane", {
  x <- acknowledgmentsSidebarPane("id")
  expect_is(x, "shiny.tag")
})

test_that("newSolutionSidebarPane", {
  x <- newSolutionSidebarPane("id", "id2")
  expect_is(x, "shiny.tag")
})

test_that("solutionResultsSidebarPane", {
  x <- solutionResultsSidebarPane("id", "id2")
  expect_is(x, "shiny.tag")
})
