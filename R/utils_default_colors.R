#' Default colors
#'
#' Generate a set of colors.
#'
#' @param x `character` vector of elements that require colors.
#'
#' @return `character` vector of colors (one for each element in `x`).
#'
#' @export
default_colors <- function(x) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(x),
    identical(anyDuplicated(x), 0L)
  )

  # prelminary processing
  n <- length(x)

  # determine colors
  if (n == 1) {
    out <- c("#37a7d9")
  } else if (length(x) >= length(.colors)) {
    ## if all colors need to be used then do so
    out <- c("", ncol = n)
    out <- c(.colors, rep("black", n - length(.colors)))
  } else {
    ## if only a subset of colors are needed, then find optimal color scheme
    rgb <- grDevices::col2rgb(.colors)
    hsv <- grDevices::rgb2hsv(rgb)
    idx <- cluster::pam(t(hsv), length(x))$id.med
    out <- .colors[idx]
  }

  # return colors
  setNames(out, x)
}

# internal character vector containing color codes
.colors <- c(
  "#d63e2a", "#a23336", "#ff8e7f", "#ef932f", "#ffc990", "#72af26",
  "#6e7d22", "#bbf970", "#37a7d9", "#00639f", "#8adaff", "#d152b8",
  "#5a386a", "#ff90e9", "#436978", "#575757", "#a3a3a3"
)
