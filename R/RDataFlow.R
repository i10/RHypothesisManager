#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export
RDataFlow <- function(message, width = NULL, height = NULL, elementId = NULL) {

  # forward options using x
  x = message

  # create widget
  htmlwidgets::createWidget(
    name = 'RDataFlow',
    x,
    width = width,
    height = height,
    package = 'RDataFlowPlugin',
    elementId = elementId
  )
}

#' Shiny bindings for RDataFlow
#'
#' Output and render functions for using RDataFlow within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a RDataFlow
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name RDataFlow-shiny
#'
#' @export
RDataFlowOutput <- function(outputId, width = '100%', height = '100%'){
  htmlwidgets::shinyWidgetOutput(outputId, 'RDataFlow', width, height, package = 'RDataFlowPlugin')
}

RDataFlow_html <- function(id, style, class, ...) {
  tag("div", list(
    id = id,
    style = style,
    class = class,
    tag("div", list(
      id="menu",
      tag("label", list(
        `for` = "toggle_strict",
        "Stop on warnings",
        tag("input", list(
          id = "toggle_strict",
          onchange = 'Shiny.setInputValue("set_strict", this.checked);',
          type = "checkbox"
        ))
      ))
    )),
    tag("div", list(
      id="selector",
      tag("h3", list("Variables")),
      tag("ul", list())
    )),
    tag("div", list(
      id = "wrapper",
      tag("svg", list())
    )),
    tag("div", list(
      id="legend",
      tag("h3", list("Hypotheses")),
      tag("ul", list())
    )),
    tag("div", list(
      id="tooltip",
      class="tooltip",
      tag("div", list())
    ))
  ))
}

#' @rdname RDataFlow-shiny
#' @export
renderRDataFlow <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, RDataFlowOutput, env, quoted = TRUE)
}
