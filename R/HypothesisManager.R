#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export
HypothesisManager <- function(message, width = NULL, height = NULL, elementId = NULL) {

  # forward options using x
  x = message

  # create widget
  htmlwidgets::createWidget(
    name = 'HypothesisManager',
    x,
    width = width,
    height = height,
    package = 'HypothesisManagerPlugin',
    elementId = elementId
  )
}

#' Shiny bindings for HypothesisManager
#'
#' Output and render functions for using HypothesisManager within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a HypothesisManager
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name HypothesisManager-shiny
#'
#' @export
HypothesisManagerOutput <- function(outputId, width = '100%', height = '100%'){
  htmlwidgets::shinyWidgetOutput(outputId, 'HypothesisManager', width, height, package = 'HypothesisManagerPlugin')
}

HypothesisManager_html <- function(id, style, class, ...) {
  tag("div", list(
    id = id,
    style = style,
    class = class,
    tag("div", list(
      id="selector",
      tag("h3", list("Variables")),
      tag("ul", list())
    )),
    tag("div", list(
      id = "wrapper",
      tag("svg", list()),
      actionButton("edit", "Inject data in place", icon = icon("edit")),
      actionButton("copy", "Inject data into a clone", icon = icon("copy"))
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

#' @rdname HypothesisManager-shiny
#' @export
renderHypothesisManager <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, HypothesisManagerOutput, env, quoted = TRUE)
}


simpleCheckbox <- function (id, label, value = FALSE, inline = FALSE, ...) {
  tags$label(
    class = if (inline) "checkbox-inline" else "checkbox",
    `for` = id,
    tags$input(
      id = id,
      type = "checkbox",
      checked = if (value) "checked" else NULL
    ),
    tags$span(label),
    ...
  )
}


hypothesisEditor <- function (hypothesis, title, action_id, variables) {
  columns <- list("New column" = "")

  for (id in variables[variables$type == "column", ]$id) {
    col_name <- variables[variables$id == id, ]$name
    dataset_name <- variables[sapply(variables$columns, function (columns) id %in% columns ), ]$name

    name <- paste(c(col_name, "|", dataset_name), collapse = " ")

    columns[[name]] <- id
  }

  modalDialog(
    title = title,
    if (!is.null(hypothesis$columns[[1]]$dependant)) {
      var <- as.list(variables[variables$id == hypothesis$columns[[1]]$dependant, ])
        tagList(
        selectizeInput(var$id, var$name, choices = columns, selected = var$id,
                       options = list("create" = TRUE)),
        tags$hr()
      )
    },
    tagList(list = lapply(hypothesis$columns[[1]]$control, function (col_id) {
      var <- as.list(variables[variables$id == col_id, ])
      selectizeInput(var$id, var$name, choices = columns, selected = var$id,
                     options = list("create" = TRUE))
    })),
    footer = tagList(
      tags$p("Please note: replacement is made with a regex,", tags$br(), "not with model reconstruciton", class = "footnote"),
      modalButton("Cancel"),
      actionButton(action_id, "OK")
    )
  )
}


addin <- function () {
  library(miniUI)
  library(rstudioapi)
  library(shiny)

  options(stringsAsFactors = FALSE)

  eval_ <<- FALSE
  strict <<- FALSE
  pause <<- FALSE

  ui = bootstrapPage(
    gadgetTitleBar("",
      left = tags$div(
        actionButton("execute", "Execute selection", icon = icon("play")),
        simpleCheckbox("strict", "Stop on warnings", value = strict, inline = TRUE),
        simpleCheckbox("eval", "Shallow evaluation", value = !eval_, inline = TRUE,
                       title = "Only analyze textual content of the script. Uncheck to evaluate the variable values")
      ),
      right = tags$div(
        simpleCheckbox("pause", "Pause", value = pause, inline = TRUE),
        actionButton("exit", "Exit")
      )
    ),
    HypothesisManagerOutput("graph")
  )

  failing_context_notification_id <<- NULL
  last_error_id <<- NULL

  server <- function (input, output, session) {
    invalidatePeriodically <- reactiveTimer(intervalMs = 1000)

    old_path <- ""
    old_hash <- ""
    old_text <- ""

    variables <- NULL
    functions <- NULL
    hypotheses <- NULL

    observe(
      {
        invalidatePeriodically()

        if (pause)
          return()

        tryCatch(
          expr = {
            c(id, path, textContents, selections) %<-% getSourceEditorContext()[0:4]

            if (!is.null(failing_context_notification_id)) {
              removeNotification(failing_context_notification_id)
              failing_context_notification_id <<- NULL
            }

            if (!isTruthy(textContents)) {
              pause <<- TRUE
              updateCheckboxInput(getDefaultReactiveDomain(), "pause", value = pause)
              return()
            }

            if (isTruthy(dirname(path)))
              setwd(dirname(path))

            file_name <- basename(path)
            hash <- digest::digest(textContents, "md5")

            if (path != old_path) {
              variables <- NULL
              functions <- NULL
              hypotheses <- NULL
            }

            if (path != old_path || hash != old_hash) {
              tryCatch(
                expr = withCallingHandlers(
                  expr = {
                    too_many_changes <- path != old_path || !isTruthy(old_text)
                    first_line <- 1

                    vv <- variables
                    ff <- functions
                    hh <- hypotheses

                    if (!too_many_changes) {
                      c(vv, ff, hh, too_many_changes, first_line) %<-% parse_diff(textContents, old_text, variables, functions, hypotheses)
                    }

                    if (too_many_changes) {
                      withProgress(
                        expr = c(vv, ff, hh) %<-% parse(textContents, first_line, interactive = TRUE,
                                                        variables=vv, functions=ff, hypotheses=hh),
                        min = first_line,
                        max = length(textContents),
                        message = paste0(c("Loading", file_name), collapse = " ")
                      )
                    }

                    variables <<- vv
                    functions <<- ff
                    hypotheses <<- hh

                    # Clear the undumpable values from the variables list
                    dumpability_selector = is.na(vv$value) | apply(vv, 1, function (v) tryCatch(expr = { jsonlite::toJSON(v); TRUE }, error = function(...) FALSE))

                    if (!all(dumpability_selector))
                      vv[!dumpability_selector, ]$value <- NA

                    output$graph <- renderHypothesisManager({
                      HypothesisManager(list(
                        variables = unname(apply(vv, 1, as.list)),
                        functions = unname(apply(ff, 1, as.list)),
                        hypotheses = unname(apply(hh, 1, as.list))
                      ))
                    })
                  },
                  warning = function (w) {
                    action = NULL

                    if (grepl("there is no package", w$message)) {
                      lib = regmatches(w$message, regexec("package called ‘(\\w+)’", w$message, perl = TRUE))[[1]][2]

                      action = tags$a(href='#',
                                      onclick=paste0(c('Shiny.setInputValue("install", ', jsonlite::toJSON(list(name=lib, lines=w$lines), auto_unbox = TRUE), ');'), collapse = ""),
                                      paste0(c("Install the `", lib, "` package"), collapse = ""))
                    }

                    showNotification(w$message, action = action, type = "warning")
                  }
                ),

                error = function (e) {
                  action = NULL

                  if (grepl("there is no package", e$message)) {
                    lib = regmatches(e$message, regexec("package called ‘(\\w+)’", e$message, perl = TRUE))[[1]][2]

                    action = tagList(
                      tags$a(href='#',
                             onclick=paste0(c('Shiny.setInputValue("install", ', jsonlite::toJSON(list(name=lib, lines=e$lines), auto_unbox = TRUE), ');'), collapse = ""),
                             paste0(c("Install the `", lib, "` package"), collapse = "")),
                      tags$br(),
                      tags$a(href='#',
                             onclick=paste0(c('Shiny.setInputValue("comment", ', jsonlite::toJSON(e$lines), ');'), collapse = ""),
                             paste0(c("Comment the lines ", e$lines[1], ":", e$lines[2]), collapse = ""))
                    )

                  } else if ("custom_code" %in% names(e)) {
                    action = tagList(
                      tags$a(href='#',
                             onclick=paste0(c('Shiny.setInputValue("goto", ', jsonlite::toJSON(e$lines), ');'), collapse = ""),
                             "Inspect"),
                      tags$br(),
                      tags$a(href='#',
                             onclick=paste0(c('Shiny.setInputValue("comment", ', jsonlite::toJSON(e$lines), ');'), collapse = ""),
                             paste0(c("Comment the lines ", e$lines[1], ":", e$lines[2]), collapse = ""))
                    )

                  } else if ("lines" %in% names(e)) {
                    # TODO: split into the debug mode with goto action towards the faulting code and production mode with complete masking of internal problems?

                    e$message = paste0(c("Addin has failed to parse the file ", file_name, " \nbecause of lines ", e$lines[1], ":", e$lines[2],
                                         " triggering the following exception:\n\"", e$message, "\""), collapse = "")
                  }

                  last_error_id <<- showNotification(e$message, action = action, type = "error", duration = NA)
                },

                finally = {
                  old_path <<- path
                  old_hash <<- hash
                  old_text <<- textContents
                }
              )
            }
          },
          error = function (e) {
            if (is.null(failing_context_notification_id)) {
              failing_context_notification_id <<- showNotification(
                paste0(c("Error getting context from RStudio:", e$message),
                       collapse = " "),
                type = "error",
                duration = NA
              )
            }
          }
        )
      }
    )

    observeEvent(
      input$goto,
      {
        c(line_no1, line_no2) %<-% lapply(input$goto, as.integer)

        setCursorPosition(document_position(line_no1, 0))

        setSelectionRanges(
          list(document_range(c(line_no1, 0), c(line_no2, Inf)))
        )
      }
    )

    observeEvent(
      input$select,
      {
        selection_ranges <- lapply(
          functions[functions$id %in% input$select, ]$lines,
          function (lines) document_range(c(lines[[1]], 0), c(lines[[2]], Inf))
        )

        setSelectionRanges(selection_ranges)
      }
    )

    observeEvent(
      input$comment,
      {
        c(line_no1, line_no2) %<-% lapply(input$comment, as.integer)

        insertText(
          lapply(line_no1:line_no2, function (line) document_position(line, -1L)),
          "# "
        )

        setCursorPosition(document_position(line_no1, -1L))

        removeNotification(last_error_id)
      }
    )

    observeEvent(
      input$exit,
      {
        stopApp()
      }
    )

    observeEvent(
      input$execute,
      {
        c(id, path, textContents, selections) %<-% getSourceEditorContext()[0:4]

        for (selection in selections) {
          print(
            tryCatch(
              expr = {
                exp <- base::parse(text = selection$text)

                eval(exp, envir = env)
              },
              error = function (e) e,
              warning = function (w) w
            )
          )
        }
      }
    )

    observeEvent(
      input$strict,
      {
        strict <<- input$strict
      }
    )

    observeEvent(input$eval, {
      if (input$eval == eval_) {
        eval_ <<- !input$eval

        old_hash <<- ""
      }
    })

    observeEvent(
      input$help,
      {
        c(package, func_name) %<-% input$help

        print(help(func_name, package = tail(strsplit(package, ":")[[1]], n = 1)))
      }
    )

    observeEvent(
      input$edit_hypothesis,
      {
        hypothesis <- as.list(hypotheses[hypotheses$id == input$edit_hypothesis, ])

        showModal(hypothesisEditor(
          hypothesis = hypothesis,
          title = paste0(c("Edit hypothesis", hypothesis$name), collapse = " "),
          action_id = "replace_hypothesis",
          variables = variables
        ))
      }
    )

    observeEvent(
      input$replace_hypothesis,
      {
        removeModal()

        mapping <- list()

        for (name in names(input))
          if (startsWith(name, "v-")) {
            var <- as.list(variables[variables$id == name, ])

            if (startsWith(input[[name]], "v-")) {
              mapping[var$name] <- variables[variables$id == input[[name]], ]$name

            } else {
              mapping[var$name] <- input[[name]]
            }
          }

        function_selector <- functions$id %in% hypotheses[hypotheses$id == input$edit_hypothesis, ]$functions[[1]]

        t <- FALSE
        for (i in 1:nrow(functions)) {
          if (function_selector[i] && functions[i, ]$depth > 1) {
            t <- TRUE
            function_selector[i] <- FALSE

          } else if (t && functions[i, ]$depth == 1) {
            function_selector[i] <- TRUE
            t <- FALSE
          }
        }

        if (any(function_selector)) {
          ff = functions[function_selector, ]

          for (i in 1:nrow(ff)) {
            func <- as.list(ff[i,])

            signature <- func$signature
            range <- document_range(c(func$lines[[1]][[1]], 0), c(func$lines[[1]][[2]], Inf))

            for (old_name in names(mapping))
              signature <- gsub(old_name, mapping[old_name], signature)

            insertText(range, signature)
          }
        }
      }
    )

    observeEvent(
      input$edit,
      {
        first_line <- subset(functions, id %in% input$select)$lines[[1]][[1]]

        prior_functions <- subset(functions,       sapply(lines, function (lines) lines[[2]]) < first_line)

        prior_variables <- subset(variables,       type == "column" | origin %in% prior_functions$id)
        prior_variables <- subset(prior_variables, type != "column" | id %in% unlist(prior_variables$columns))

        showModal(hypothesisEditor(
          hypothesis = as.list(hypotheses[hypotheses$id == input$copy_hypothesis, ]),
          title = "Edit",
          action_id = "replace_selection",
          variables = prior_variables
        ))
      }
    )

    observeEvent(
      input$replace_selection,
      {
        removeModal()

        mapping <- list()

        for (name in names(input))
          if (startsWith(name, "v-")) {
            var <- as.list(variables[variables$id == name, ])

            if (startsWith(input[[name]], "v-")) {
              mapping[var$name] <- variables[variables$id == input[[name]], ]$name

            } else {
              mapping[var$name] <- input[[name]]
            }
          }

        ff = subset(functions, id %in% input$select)

        if (nrow(ff)) {
          for (i in 1:nrow(ff)) {
            func <- as.list(ff[i,])

            signature <- func$signature
            range <- document_range(c(func$lines[[1]][[1]], 0), c(func$lines[[1]][[2]], Inf))

            for (old_name in names(mapping))
            signature <- gsub(old_name, mapping[old_name], signature)

            insertText(range, signature)
          }
        }
      }
    )

    observeEvent(
      input$copy,
      {
        first_line <- subset(functions, id %in% input$select)$lines[[1]][[1]]

        prior_functions <- subset(functions,       sapply(lines, function (lines) lines[[2]]) < first_line)

        prior_variables <- subset(variables,       type == "column" | origin %in% prior_functions$id)
        prior_variables <- subset(prior_variables, type != "column" | id %in% unlist(prior_variables$columns))

        showModal(hypothesisEditor(
          hypothesis = as.list(hypotheses[hypotheses$id == input$copy_hypothesis, ]),
          title = "Copy",
          action_id = "paste",
          variables = prior_variables
        ))
      }
    )

    observeEvent(
      input$paste,
      {
        removeModal()

        mapping <- list()

        for (name in names(input))
          if (startsWith(name, "v-")) {
            var <- as.list(variables[variables$id == name, ])

            if (startsWith(input[[name]], "v-")) {
              mapping[var$name] <- variables[variables$id == input[[name]], ]$name

            } else {
              mapping[var$name] <- input[[name]]
            }
          }

        function_selector <- functions$id %in% input$select

        last_func_index <- Position(function (x) x, function_selector, right = TRUE)

        breakpoint_function <- functions[1:nrow(functions) > last_func_index & !is.na(functions$breakpoint), ]

        if (nrow(breakpoint_function))
          row <- breakpoint_function[1, ]$lines[[1]][[1]] - 1

        else
          row <- Inf

        position <- document_position(row, 0)

        if (any(function_selector)) {
          insertText(position, "\n")

          ff <- functions[function_selector, ]

          for (signature in (if (row == Inf) ff$signature else rev(ff$signature))) {
            for (old_name in names(mapping))
              signature <- gsub(old_name, mapping[old_name], signature)

            insertText(position, paste0(c(signature, "\n"), collapse = ""))
          }
        }

        # TODO: select the newly pasted functions
        setCursorPosition(position)
      }
    )

    observeEvent(
      input$install,
      {
        tryCatch(expr = {
          lines <- input$install$lines
          lib <- input$install$name

          install.packages(lib)

          removeNotification(last_error_id)

          library(lib, character.only = TRUE)

          showNotification(paste0(c("Successfully installed `", lib, "`"), collapse =""))

          old_hash <<- ""
        },
        error = function (e) {
          last_error_id <<- showNotification(
            e$message,
            action=tags$a(href='#',
                          onclick=paste0(c('Shiny.setInputValue("comment", ', jsonlite::toJSON(lines), ');'), collapse = ""),
                          paste0(c("Comment the lines ", lines[1], ":", lines[2]), collapse = "")),
            type = "error",
            duration = NA
          )
        })
      }
    )
  }

  viewer <- paneViewer(300)

  runGadget(ui, server, viewer = viewer)
}
