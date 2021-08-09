#' Has HTML node?
#'
#' Check if a node with a pre-determined identifier is present within HTML
#' code.
#'
#' @param x `character` HTML code.
#'
#' @param id `character` desired (`"id"`) identifier.
#'
#' @param class `character` (optional) desired class attribute for node.
#'
#' @return `logical` indicating if a node with the specified identifier
#'   was found.
#'
#' @export
has_html_node <- function(x, id, class = NA_character_) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, c("shiny.tag", "shiny.tag.list")),
    assertthat::is.string(id),
    assertthat::noNA(id),
    assertthat::is.string(class)
  )
  # parse HTML
  h <- xml2::read_html(as.character(x))
  # subset nodes to desired classes
  if (is.na(class)) {
    h <- xml2::xml_find_all(h, paste0("//*[@id='", id, "']"))
  } else {
    h <- xml2::xml_find_all(h, paste0(
      "//*[@id='", id,
      "' and contains(@class, '", class, "')]"
    ))
  }
  # throw warning if multiple found
  if (length(h) > 1) {
    warning(paste0("multiple nodes found with id=\"", id, "\""))
  }
  # see if h contains at least one element
  length(h) > 0
}

#' Application parameters
#'
#' Load Action App parameters for testing functions.
#'
#' @return `list` object with parameters.
#'
#' @export
app_parameters <- function() {
  path <- system.file("extdata", "config.toml", package = "actionmisc")
  RcppTOML::parseTOML(path)
}
