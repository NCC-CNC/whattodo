context("prepare_for_shapefile_import")

test_that("works", {
  # create file names for testing
  f1 <- tempfile(fileext = ".shp")
  f2 <- tempfile(fileext = ".shx")
  f3 <- tempfile(fileext = ".dbf")
  f4 <- tempfile(fileext = ".prj")
  input_path <- c(f1, f2, f3, f4)
  # create files based on file names
  invisible(lapply(input_path, function(x) {
    cat(x, file = x)
  }))
  # rename files
  output_path <- prepare_for_shapefile_import(input_path)
  # tests
  expect_true(all(file.exists(output_path)))
  testthat::testthat_print(output_path)
  expect_equal(length(unique(tools::file_path_sans_ext(output_path))), 1)
  expect_equal(tools::file_ext(input_path), tools::file_ext(output_path))
})
