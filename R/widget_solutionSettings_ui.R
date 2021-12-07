#' @include internal.R
NULL

#' Solution settings widget
#'
#' Constructs a widget for managing the settings for generating solutions.
#'
#' @param x [Project] object.
#'
#' @param elementId `character` HTML identifier for the widget.
#'   Defaults to `NULL`.
#'
#' @param width `character` width of the displayed widget.
#'   Defaults to `NULL` such that the widget is automatically sized.
#'
#' @param height `character` width of the displayed widget.
#'   Defaults to `NULL` such that the widget is automatically sized.
#'
#' @section Server value:
#' The widget sends a `list` with the following values to the server:
#'
#'
#' \describe{
#'
#' \item{id}{`character` identifier for the theme or weight.}
#'
#' \item{setting}{`character` name of the updated setting.
#'   Available options include: `"factor"` or `"goal"`.}
#'
#' \item{value}{new `numeric` values.}
#'
#' \item{type}{`character` indicating if the updated setting corresponds
#'   to a `theme` or `weight`.}
#'
#' }
#'
#' The widget contains two buttons. The server value for these
#' buttons are an `integer` indicating the number of times they
#' has been clicked. They can be queried using `id_start_button` and
#' `id_stop_button` where `id` is the argument to `elementId`.
#'
#' @examples
#' \dontrun{
#' # run Shiny app to demo the sidebar pane
#' if (interactive()) {
#'   runExample("solutionSettings")
#' }
#' }
#'
#' @rdname solutionSettings-widget
#'
#' @export
solutionSettings <- function(x, width = NULL, height = NULL, elementId = NULL) {
  # assert arguments are valid
  assertthat::assert_that(inherits(x, "Project"))

  # prepare data
  p <- x$get_solution_settings_data()
  p$api <- list() # enable API

  # create widget
  htmlwidgets::createWidget(
    name = "solutionSettings",
    p,
    width = width,
    height = height,
    package = "whattodo",
    elementId = elementId,
    dependencies = c(
      htmltools::htmlDependencies(shiny::icon("map-marked-alt")),
      htmltools::htmlDependencies(
        shinyBS::bsCollapse(shinyBS::bsCollapsePanel("id"))
      )
    )
  )
}

#' Shiny bindings for `solutionSettings`
#'
#' Use `solutionSettingsOutput()` to create a user interface element,
#' and `renderSolutionSettings()` to render the widget.
#'
#' @param outputId output variable to read from
#'
#' @param width,height Must be a valid CSS unit (like \code{"100\%"},
#'   \code{"400px"}, \code{"auto"}) or a number, which will be coerced to a
#'   string and have \code{"px"} appended.
#'
#' @param expr An expression that generates a [solutionSettings()].
#'
#' @param env The environment in which to evaluate \code{expr}.
#'
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name solutionSettings-shiny
#'
#' @export
solutionSettingsOutput <- function(outputId, width = "100%", height = "auto") {
  htmlwidgets::shinyWidgetOutput(
    outputId, "solutionSettings", width, height,
    package = "whattodo"
  )
}

#' @rdname solutionSettings-shiny
#' @export
renderSolutionSettings <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  htmlwidgets::shinyRenderWidget(
    expr, solutionSettingsOutput, env,
    quoted = TRUE
  )
}

# Add custom HTML for the widget (automatically used by htmlwidgets)
solutionSettings_html <- function(id, style, class, ...) {
  # HTML scaffold
  x <-
    htmltools::tags$div(
      id = id, class = class, style = style,
      htmltools::div(
        class = "solution-settings",
        shinyBS::bsCollapse(
          id = paste0(id, "_collapse"),
          multiple = FALSE,
          open = paste0(id, "_collapseThemePanel"),
          shinyBS::bsCollapsePanel(
            title = htmltools::tags$span(
              shinyBS::tipify(
                el = htmltools::tags$span(
                  shiny::icon("star"),
                  "Goals"
                ),
                title = paste(
                  "Themes describe facets of biodiversity that are important",
                  "for conservation (e.g. species, habitats, ecosystems).",
                  "To help safeguard them,",
                  "you can set goals for themes to increase the expected",
                  "amount that would result following a solution."
                ),
                options = list(container = "body")
              )
            ),
            value = paste0(id, "_collapseThemePanel"),
            htmltools::tags$div(
              class = "panel-content-inner",
              htmltools::tags$div(class = "themes")
            )
          ),
          shinyBS::bsCollapsePanel(
            title = htmltools::tags$span(
              shinyBS::tipify(
                el = htmltools::tags$span(
                  shiny::icon("weight-hanging"),
                  "Weights"
                ),
                title = paste(
                  "Weights describe the relative importance of meeting",
                  "goals for each of the themes."
                ),
                options = list(container = "body")
              )
            ),
            value = paste0(id, "_collapseWeightPanel"),
            htmltools::tags$div(
              class = "panel-content-inner",
              htmltools::tags$div(class = "weights")
            )
          )
        )
      ),

      ### footer
      htmltools::tags$div(
        class = "solution-footer",
        htmltools::tags$div(
          class = "solution-footer-start-button",
          `data-toggle` = "tooltip",
          `data-placement` = "top",
          `data-container` = "body",
          `data-trigger` = "hover",
          title = paste(
            "Generate a new solution"
          ),
          shinyFeedback::loadingButton(
            inputId = paste0(id, "_start_button"),
            label = "Optimize!",
            class = "btn btn-primary",
            loadingLabel = "",
            style = "width: 86px;"
          ),
          htmltools::tags$div(
            class = "solution-footer-stop-button",
            `data-toggle` = "tooltip",
            `data-placement` = "top",
            `data-container` = "body",
            `data-trigger` = "hover",
            title = "Stop optimizing",
            shinyBS::bsButton(
              inputId = paste0(id, "_stop_button"),
              label = "",
              icon = shiny::icon("ban"),
              style = "danger"
            )
          )
        )
      )
    )

  # add HTML template scaffolds for dynamic content
  ## parameter
  x <-
    htmltools::tagAppendChild(
      x,
      htmltools::tags$template(
        class = "parameter-setting-template",
        htmltools::tags$div(
          class = paste("parameter-setting solution-setting"),
          header_component_scaffold("parameter"),
          htmltools::tags$div(
            class = "parameter-slider",
            `data-toggle` = "tooltip",
            `data-placement` = "bottom",
            `data-container` = "body",
            `data-trigger` = "hover",
            title = "Set the parameter value",
            slider_component_scaffold()
          )
        )
      )
    )

  ## weight
  x <-
    htmltools::tagAppendChild(
      x,
      htmltools::tags$template(
        class = "weight-setting-template",
        htmltools::tags$div(
          class = paste("weight-setting solution-setting"),
          header_component_scaffold("weight"),
          htmltools::tags$div(
            class = "weight-slider",
            `data-toggle` = "tooltip",
            `data-placement` = "bottom",
            `data-container` = "body",
            `data-trigger` = "hover",
            title = "Set the factor",
            slider_component_scaffold()
          ),
        )
      )
    )

  ## theme
  x <-
    htmltools::tagAppendChild(
      x,
      htmltools::tags$template(
        class = "theme-setting-template",
        htmltools::tags$div(
          class = "theme-setting solution-setting",
          header_component_scaffold("theme", reset_button = TRUE),
          goal_component_scaffold()
        )
      )
    )

  # return HTML
  x
}
