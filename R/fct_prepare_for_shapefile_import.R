#' @include internal.R
NULL

#' Prepare files for shapefile import
#'
#' This function prepares a set of files so that they can be imported as
#' a shapefile.
#'
#' @details
#' Specifically, the files are all assigned the same file name
#' (i.e. base name without the file extension) so that they form the
#' constituent files of a shapefile.
#'
#' @param x `character` file paths.
#'
#' @return `character` file paths for renamed files.
#'
#' @export
prepare_for_shapefile_import <- function(x) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(x), assertthat::noNA(x)
  )
  # main processing
  out <- vapply(x[-1], FUN.VALUE = character(1), function(i) {
    # determine new file path
    new_path <- paste0(
      file.path(
        dirname(x[[1]]),
        basename(tools::file_path_sans_ext(x[[1]]))
      ),
      ".",
      tools::file_ext(i)
    )
    # copy file to new file path
    file.copy(from = i, to = new_path)
    file.remove(i)
    # return new path
    new_path
  })
  # return new paths
  unname(c(x[1], out))
}
