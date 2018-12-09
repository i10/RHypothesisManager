library(miniUI);
#library(rstudioapi);
library(shiny);
library(uuid);


own_error <- function (message, custom_code, call = sys.call(-1), ...) {
  structure(
    class = c("own_error", "error", "condition"),
    list(message = message, call = call, custom_code = custom_code, ...)
  )
}


loop <- function (text, interactive = FALSE) {
  # Prepare
  variables <- list();

  functions <- data.frame(matrix(ncol = 8, nrow = 0));
  colnames(functions) <- c("id", "name", "lines", "signature", "packages", "arguments", "depth", "breakpoint");

  hypotheses <- data.frame(matrix(ncol = 6, nrow = 0));
  colnames(hypotheses) <- c("id", "name", "columns", "functions", "models", "formulas");

  # Run loop
  line_no1 <- 1;

  while (line_no1 < length(text) + 1) {
    line_no2 <- line_no1;
    break_ <- FALSE;

    while (!break_ && line_no2 < length(text) + 1) {
      tryCatch(
        expr = {
          line <- paste0(text[line_no1:line_no2], collapse="\n");

          row <- parse(text=paste0(text[line_no1:line_no2], collapse="\n"));

          if (!is.language(row) || length(row) == 0) {
            break;
          }

          row <- row[[1]];

          before_funcs <- nrow(functions);

          c(variables, functions, hypotheses) %<-% recursion(row, variables, functions, hypotheses)

          if (nrow(functions) > before_funcs) {
            functions[(before_funcs + 1):nrow(functions), ]$lines <- list(list(line_no1, line_no2));
            functions[nrow(functions), ]$signature <- line;
          }

          break;
        },
        warning = function (w) {
          if (strict) {
            stop(w$message);

          } else {
            warning(w);
            break_ <<- TRUE;
          }
        },
        error = function (e) {
          if (grepl("unexpected end of input", e$message)) {
            line_no2 <<- line_no2 + 1;

          } else {
            e$lines <- c(line_no1, line_no2)

            stop(e);  # error rethrow
          }
        },
        finally = function (...) {}
      )
    }

    if (interactive) {
      incProgress(line_no2 - line_no1 + 1)
    }

    line_no1 <- line_no2 + 1;
  }

  list(variables, functions, hypotheses);
}


find_hypothesis <- function (exp, hypotheses, variables, add = FALSE) {
  functions <- data.frame(matrix(ncol = 8, nrow = 0));
  colnames(functions) <- c("id", "name", "lines", "signature", "packages", "arguments", "depth", "breakpoint");

  c(variables, functions, h) %<-% recursion(exp[[length(exp)]], variables, functions, data.frame(),
                                            addition_mode = TRUE)

  con_columns <- collect_columns(variables, functions);

  con_column_ids <- lapply(con_columns, function (column) column$id);

  if (length(exp) == 3) {
    name <- paste0(as.character(exp)[c(2, 1, 3)], collapse = ' ');

    if (is.name(exp[[2]])) {
      c(dep_column_index, variables) %<-% find_variable(as.character(exp[[2]]), variables,
                                                        add = TRUE,
                                                        type_constraint = "column",
                                                        check_shadowing = FALSE);

      dep_column_id <- variables[[dep_column_index]]$id;

    } else {
      before_funcs <- nrow(functions);

      c(variables, functions, h) %<-% recursion(exp[[2]], variables, functions, data.frame(),
                                                addition_mode = TRUE)

      dep_columns <- collect_columns(variables, functions[before_funcs:nrow(functions), ]);

      dep_column_id <- dep_columns[[length(dep_columns)]]$id;
    }

    columns <- list(
      dependant = dep_column_id,
      control = con_column_ids
    )

  } else if (length(exp) == 2) {
    name <- paste0(exp, collapse = '');

    columns <- list(
      dependant = NULL,
      control = con_column_ids
    )

  } else {
    name <- '';
    columns <- NULL;
  }

  index <- match(name, hypotheses$name);

  if (is.na(index)) {
    hypothesis <- NULL;
    index <- nrow(hypotheses) + 1

  } else {
    hypothesis <- hypotheses[index, ]
  }

  if (index > nrow(hypotheses) && add) {
    hypothesis <- list(
      id =        paste0(c("h", UUIDgenerate()), collapse = "-"),
      name =      name,
      columns =   list(columns),
      functions = list(list()),
      models =    list(list()),
      formulas =  list(list())
    );

    hypotheses[index,] <- hypothesis;
  }

  list(index, hypotheses, variables);
}


collect_columns <- function (variables, functions) {
  array <- list();

  for (variable in variables) {
    for (func_args in functions$arguments) {
      if (variable$id %in% func_args) {
        if (variable$type == "column") {
          array[[length(array) + 1]] <- variable;

        } else if (variable$type == "constant") {
          # TODO: somehow set the "column" type to the variable?
          # TODO: if yes: which data variable it should be assigned to?
          #               or can it just wait until later?
          #               or it won't be any different from the old workflow with subsequent update_hypothesis call?
          array[[length(array) + 1]] <- variable;

        } else if (variable$type == "formula") {
          # TODO: `col1 ~ col2 ~ cols` case?
        }
      }
    }
  }

  array;
}


find_variable <- function (name, variables, add = FALSE, force = FALSE, type_constraint = NULL,
                           check_shadowing = TRUE) {
  var <- NULL;
  index <- length(variables) + 1;

  if (!force) {
    packages <- find(name);

    # If the name can be found in the packages
    is_function_name <- !!length(packages);

    if (is_function_name && do_eval) {
      # If the name can _only_ be found in the packages
      is_function_name <- !length(setdiff(grep("package:", packages), 1:length(packages)));
    }

    if (length(variables) > 0)
      for (i in 1:length(variables)) {
        old_var <- variables[[i]];

        if (old_var$name == name && (is.null(type_constraint) || any(old_var$type == type_constraint))) {
          is_function_name <- FALSE;  # having the variable by the name overrules the function name search (probably)
          var <- old_var;
          index <- i;
        }
      }

    if (is_function_name && check_shadowing) {
      return(list(index, variables))
    }
  }

  if (index > length(variables)) {
    if (!add) {
      e <- own_error(paste0(c("The variable `", name, "` has never occured before, but addition has not been granted"), collapse=""),
                     custom_code = 1)
      stop(e)
    }

    var <- list(
      id =          paste0(c("v", UUIDgenerate()), collapse = "-"),
      name =        name,
      precursors =  list(),
      columns =     list(),
      origin =      NULL,
      type =        "constant",
      value =       NULL,
      generation =  0
    );

    variables[[index]] <- var;
  }

  list(index, variables);
}


argument_recursion <- function (args, func,
                                variables, functions, hypotheses,
                                depth, addition_mode = FALSE) {
  for (arg in args) {
    if (missing(arg)) {

    } else if (is.atomic(arg)) {
      func$arguments <- append(func$arguments, arg)

    } else if (is.name(arg)) {
      # NB: Technically, thyCatch wrapping here is a crutch, however it does not seem possbile to ensure that
      #     the existence of the variables that are being called within in some of the cases,
      #     even though the code remains valid overall
      tryCatch(
        expr = {
          c(var_index, variables) %<-% find_variable(as.character(arg), variables,
                                                     add = addition_mode);

          if (variables[[var_index]]$type == "constant" && !is.null(variables[[var_index]]$value)) {
            func$arguments <- append(func$arguments, variables[[var_index]]$value);  # value is (supposed to be) atomic

          } else if (variables[[var_index]]$type == "formula") {
            func$arguments <- append(func$arguments, variables[[var_index]]$value);  # value is a hypothesis id

          } else {
            func$arguments <- append(func$arguments, variables[[var_index]]$id)
          }
        },
        warning = function (...) {},
        error = function (...) {},
        finally = function (...) {}
      )

    } else if (is.call(arg) && identical(arg[[1]], quote(`~`))) {
      c(hyp_index, hypotheses, variables) %<-% find_hypothesis(arg, hypotheses, variables, add = TRUE);

      func$arguments <- append(func$arguments, hypotheses[hyp_index,]$id);
      hypotheses[hyp_index,]$functions[[1]] = append(hypotheses[hyp_index,]$functions[[1]], func$id);

    } else if (is.call(arg) && (identical(arg[[1]], quote(`c`)) || identical(arg[[1]], quote(`list`)))) {
      c(func, variables, functions, hypotheses) %<-% argument_recursion(as.list(arg)[2:length(arg)], func,
                                                                        variables, functions, hypotheses,
                                                                        depth, addition_mode = addition_mode)

    } else {
      before_funcs <- nrow(functions);

      c(variables, functions, hypotheses) %<-% recursion(arg, variables, functions, hypotheses,
                                                         addition_mode = addition_mode, depth = depth);

      if (nrow(functions) != before_funcs) {
        func$arguments <- append(func$arguments, functions[nrow(functions), ]$id);
      }
    }
  }

  list(func, variables, functions, hypotheses);
}


hypothesis_subroutine <- function (exp, variables, functions, hypotheses,
                                   addition_mode = FALSE, depth = 0) {

  # data[data$col1 == value, ]$col2
  if (identical(exp[[1]], quote(`$`))) {
    data_name <- as.character(exp[[2]][[2]]);
    col2_name <- as.character(exp[[3]]);
    lookup <- exp[[2]][[3]];

  # data[data$col1 == value, "col2"]
  } else if (identical(exp[[1]], quote(`[`)) && is.character(exp[[4]])) {
    data_name <- as.character(exp[[2]]);
    col2_name <- exp[[4]];
    lookup <- exp[[3]];

  } else {
    return(list(variables, functions, hypotheses));
  }

  # data
  c(var_index, variables) %<-% find_variable(data_name, variables,
                                             add = TRUE,
                                             type_constraint = "data");

  do_force_add <- !length(variables[[var_index]]$columns);

  # col2
  c(col_index, variables) %<-% find_variable(col2_name, variables,
                                             add = TRUE,
                                             force = do_force_add,
                                             type_constraint = "column");

  variables[[col_index]]$type <- "column";

  if (variables[[col_index]]$generation == 0) {
    variables[[col_index]]$generation <- variables[[var_index]]$generation + 1;
  }

  if (!(variables[[col_index]]$id %in% variables[[var_index]]$columns)) {
    variables[[var_index]]$columns <- append(variables[[var_index]]$columns, variables[[col_index]]$id);
  }

  # data$col1 == value
  before_funcs <- nrow(functions);

  c(variables, f, hypotheses) %<-% recursion(lookup, variables, functions, hypotheses,
                                             addition_mode = addition_mode, depth = depth);

  columns <- list()

  for (func_args in f[(before_funcs + 1):nrow(f), ]$arguments) {
    for (arg in func_args) {
      for (var in variables) {
        if (var$id == arg && var$type == "column") {
          columns[[length(columns) + 1]] <- var;
          break;
        }
      }
    }
  }

  formula <- paste0(
    c(
      variables[[col_index]]$name,
      '~',
      paste0(
        lapply(columns, function (var) var$name),
        collapse = ' + '
      )
    ),
    collapse = ' '
  );

  formula <- parse(text=formula)[[1]];

  c(hyp_index, hypotheses, variables) %<-% find_hypothesis(formula, hypotheses, variables,
                                                           add = TRUE);

  func <- list(
    id =          paste0(c("f", UUIDgenerate()), collapse = "-"),
    name =        paste0(as.character(exp)[c(2, 1, 3)], collapse = ""),
    lines =       NA,
    signature =   NA,
    packages =    NA,
    arguments =   list(append(list(variables[[var_index]]$id, variables[[col_index]]$id), lapply(columns, function (var) var$id))),
    depth =       depth - addition_mode,
    breakpoint =  NA
  );

  hypotheses[hyp_index, ]$functions[[1]] <- append(hypotheses[hyp_index, ]$functions[[1]], func$id);

  functions[nrow(functions) + 1, ] <- func;

  list(variables, functions, hypotheses);
}


recursion <- function (exp, variables, functions, hypotheses,
                       addition_mode = FALSE, force_as_function = FALSE,
                       depth = 0) {
  depth <- depth + 1;

  # Is assignment line
  if (is.call(exp) && (identical(exp[[1]], quote(`<-`)) || identical(exp[[1]], quote(`=`)))) {
    is_mutation <- FALSE;

    if (is.call(exp[[2]])) {
      is_mutation <- TRUE;

      c(variables, tmp, hypotheses) %<-% recursion(exp[[2]], variables, functions, hypotheses,
                                                   addition_mode = TRUE, depth = depth)

      var_name <- exp[[2]];

      while (!is.name(var_name)) {
        var_name <- var_name[[2]];
      }

      var_name <- as.character(var_name);

    } else {
      var_name <- as.character(exp[[2]]);
    }

    before_funcs <- nrow(functions);

    # Find the precursors and invoked functions
    c(variables, functions, hypotheses) %<-% recursion(exp[[3]], variables, functions, hypotheses,
                                                       addition_mode = TRUE, depth = depth);

    var_geneneration <- 0;
    precursor_variable_ids <- list();

    for (variable in variables) {
      for (func_args in functions[(before_funcs + 1):nrow(functions), ]$arguments) {
        if (variable$id %in% func_args) {
          if (variable$type == "column") {
            # TODO: do smth?

          } else if (variable$name == var_name) {
            is_mutation <- TRUE;

          } else if (!(variable$id %in% precursor_variable_ids)) {
            if (var_geneneration < variable$generation + 1) {
              var_geneneration <- variable$generation + 1;
            }

            precursor_variable_ids <- append(precursor_variable_ids, variable$id);
          }
        }
      }
    }

    # Get or create the variable
    c(var_index, variables) %<-% find_variable(var_name, variables,
                                               add = TRUE,
                                               force = !is_mutation,
                                               type_constraint = c("data", "model", "constant"));

    if (is.null(variables[[var_index]]$origin)) {
      variables[[var_index]]$origin <- functions[nrow(functions), ]$id;
    }

    if (is_mutation) {
      functions[nrow(functions), ]$breakpoint <- variables[[var_index]]$id;

    } else {
      variables[[var_index]]$generation <- var_geneneration;
      variables[[var_index]]$precursors <- precursor_variable_ids;
    }

    # We can meaningfully assume that the variable is a dataframe if it is actually declared as one
    #   or infer it from the fact it has been read from the file source.
    if (functions[nrow(functions), ]$name %in% list("subset", "[", "data.frame", "as.data.frame", "table") ||
        startsWith(functions[nrow(functions), ]$name, "read")) {
      variables[[var_index]]$type <- "data";

    } else if (before_funcs - nrow(functions) == 1 &&
               functions[nrow(functions), ]$name == "~") {
      selector <- apply(hypotheses, 1, function (hyp) functions[nrow(functions)]$id %in% hyp$functions);

      variables[[var_index]]$value <- hypotheses[selector, ]$id;
      variables[[var_index]]$type <- "formula";

      hypotheses[selector, ]$formulas[[1]] = append(hypotheses[selector, ]$formulas[[1]], variables[[var_index]]$id);

    } else if (nrow(hypotheses)) {
      for (func_id in functions[(before_funcs + 1):nrow(functions), ]$id) {
        selector <- apply(hypotheses, 1, function (hyp) func_id %in% hyp$functions);

        if (nrow(hypotheses[selector, ])) {
          variables[[var_index]]$type <- "model";

          for (hyp_id in hypotheses[selector, ]$id) {
            hypotheses[hypotheses$id == hyp_id, ]$models[[1]] <- append(hypotheses[hypotheses$id == hyp_id, ]$models[[1]],
                                                                        variables[[var_index]]$id)
          }
        }

        func_args = functions[functions$id == func_id, ]$arguments[[1]];

        if (nrow(hypotheses[hypotheses$id %in% func_args, ])) {
          variables[[var_index]]$type <- "model";

          for (hyp_id in hypotheses[hypotheses$id %in% func_args, ]$id) {
            hypotheses[hypotheses$id == hyp_id, ]$models[[1]] <- append(hypotheses[hypotheses$id == hyp_id, ]$models[[1]],
                                                                        variables[[var_index]]$id)
          }
        }
      }
    }

    if (variables[[var_index]]$type == "constant") {
      tryCatch(
        expr = {
          variables[[var_index]]$value <- eval(exp[[3]]);
        },
        warning = function (...) {},
        error = function (...) {},
        finally = {}
      )
    }

  # Is "[" call
  } else if (is.call(exp) && identical(exp[[1]], quote(`[`)) && !force_as_function) {
    # data[data$col1 == value, "col2"]
    if (length(exp) == 4) {
      col_name <- exp[[4]];

      # data[data$col1 == value, ] <- but w/o the $ in the end
      if (missing(col_name)) {
        c(variables, functions, hypotheses) %<-% recursion(exp, variables, functions, hypotheses,
                                                           addition_mode = addition_mode, depth = depth,
                                                           force_as_function = TRUE);

      } else {
        c(variables, functions, hypotheses) %<-% hypothesis_subroutine(exp, variables, functions, hypotheses,
                                                                       addition_mode = addition_mode, depth = depth);
      }

    } else {
      c(variables, functions, hypotheses) %<-% recursion(exp[[2]], variables, functions, hypotheses,
                                                         addition_mode = addition_mode, depth = depth);
    }

  # Is "$" call
  } else if (is.call(exp) && identical(exp[[1]], quote(`$`))) {
    if (is.call(exp[[2]])) {
      # data[data$col1 == value, ]$col2
      if (identical(exp[[2]][[1]], quote(`[`))) {
        c(variables, functions, hypotheses) %<-% hypothesis_subroutine(exp, variables, functions, hypotheses,
                                                                       addition_mode = addition_mode, depth = depth);

      # func()$column
      } else {
        c(variables, functions, hypotheses) %<-% recursion(exp[[2]], variables, functions, hypotheses,
                                                           addition_mode = addition_mode, depth = depth - 1);
      }

    # data$column
    } else {
      c(var_index, variables) %<-% find_variable(as.character(exp[[2]]), variables,
                                                 add = addition_mode,
                                                 type_constraint = c("data", "model", "constant"));

      c(col_index, variables) %<-% find_variable(as.character(exp[[3]]), variables,
                                                 add = TRUE,
                                                 force = !length(variables[[var_index]]$columns),
                                                 type_constraint = "column");

      variables[[col_index]]$generation <- variables[[var_index]]$generation + 1;
      variables[[col_index]]$type <- "column";

      if (!(variables[[col_index]]$id %in% variables[[var_index]]$columns)) {
        variables[[var_index]]$columns <- append(variables[[var_index]]$columns, variables[[col_index]]$id)
      }

      func <- list(
        id =          paste0(c("f", UUIDgenerate()), collapse = "-"),
        name =        paste0(as.character(exp)[c(2, 1, 3)], collapse = ""),
        lines =       NA,
        signature =   NA,
        packages =    NA,
        arguments =   list(list(variables[[var_index]]$id, variables[[col_index]]$id)),
        depth =       depth - addition_mode,
        breakpoint =  NA
      );

      functions[nrow(functions) + 1, ] <- func;
    }

  # Is "~" call -- formula (aka hypothesis) initialization
  } else if (is.call(exp) && identical(exp[[1]], quote(`~`)) && !force_as_function) {
    c(index, hypotheses, variables) %<-% find_hypothesis(exp, hypotheses, variables, add = TRUE);

    c(variables, functions, hypotheses) %<-% recursion(exp, variables, functions, hypotheses,
                                                       addition_mode = addition_mode, depth = depth,
                                                       force_as_function = TRUE);


    hypotheses[index, ]$functions[[1]] <- append(hypotheses[index, ]$functions[[1]], functions[nrow(functions), ]$id);

  # Is function declaration
  } else if (is.call(exp) && identical(exp[[1]], quote(`function`))) {
    # TODO: introduce scopes

    # functions[[length(functions) + 1]] <- exp;

  # Is library import call
  } else if (is.call(exp) && (identical(exp[[1]], quote(`library`)) || identical(exp[[1]], quote(`require`))) && !force_as_function) {
    eval(exp);

  # Is library name call
  } else if (is.call(exp) && identical(exp[[1]], quote(`::`))) {
    # Shouldn't really ever pop up
    # TOOD: use to hint the exact package the function is being imported from?

  # Is paranthesis (?!)
  } else if (is.call(exp) && identical(exp[[1]], quote(`(`))) {
    c(variables, functions, hypotheses) %<-% recursion(exp[[2]], variables, functions, hypotheses,
                                                       addition_mode = addition_mode, depth = depth);

  # Is if/for clause
  } else if (is.call(exp) && (identical(exp[[1]], quote(`if`)) || identical(exp[[1]], quote(`for`)))) {
    # TODO: add conditional as the precursor?

    c(variables, functions, hypotheses) %<-% recursion(exp[[3]], variables, functions, hypotheses,
                                                       addition_mode = addition_mode, depth = depth);

  # Is block statement
  } else if (is.call(exp) && identical(exp[[1]], quote(`{`))) {
    for (line in as.list(exp)[2:length(exp)])
      c(variables, functions, hypotheses) %<-% recursion(line, variables, functions, hypotheses,
                                                       addition_mode = addition_mode, depth = depth);

  # Is a generic function call
  } else if (is.call(exp)) {
    if (is.call(exp[[1]])) {
      func_name <- paste0(as.character(exp[[1]])[c(2, 1, 3)], collapse = "");

    } else {
      func_name <- as.character(exp[[1]]);
    }

    packages <- find(func_name);

    packages <- as.list(packages[grep("package:", packages)])

    if (length(packages)) {
      packages <- list(packages);

    } else {
      packages <- NA;
    }

    func <- list(
      id =          paste0(c("f", UUIDgenerate()), collapse = "-"),
      name =        func_name,
      lines =       NA,
      signature =   paste0(c(as.character(exp[[1]]), "(" , paste0(as.character(exp[2:length(exp)]), collapse = ", "), ")"), collapse = ""),
      packages =    packages,
      arguments =   list(),
      depth =       depth - addition_mode,
      breakpoint =  NA
    );

    if (length(exp) > 1) {
      c(func, variables, functions, hypotheses) %<-% argument_recursion(as.list(exp)[2:length(exp)], func,
                                                                        variables, functions, hypotheses,
                                                                        depth = depth, addition_mode = addition_mode);

      # TODO: improve the hypothesis selection
      if (nrow(hypotheses)) {
        col_ids <- list();

        for (var in variables) {
          if (var$id %in% func$arguments && var$type == "column") {
            col_ids <- append(col_ids, var$id);
          }
        }

        if (length(col_ids)) {
          for (hyp_index in 1:nrow(hypotheses)) {
            hypothesis <- as.list(hypotheses[hyp_index, ]);

            columns <- append(hypothesis$columns[[1]]$control, hypothesis$columns[[1]]$dependant);

            if (length(setdiff(columns, col_ids)) == 0) {
              hypothesis$functions[[1]] <- append(hypothesis$functions[[1]], func$id);

              hypotheses[hyp_index, ] <- hypothesis;
              break;
            }
          }
        }
      }
    }

    func$arguments <- list(func$arguments);

    functions[nrow(functions) + 1, ] <- func;


  # Is variable name call
  } else if (is.name(exp)) {
    # print(as.character(exp));

    # NB: does not add the variable to the list by deault
    c(tmp, variables) %<-% find_variable(as.character(exp), variables,
                                         add = addition_mode);

  # Is atomic
  } else if (is.atomic(exp)) {

  # Dunno
  } else {

  }

  list(variables, functions, hypotheses)
}


addin <- function () {
  options(stringsAsFactors = FALSE);

  ui = bootstrapPage(RDataFlowOutput("graph"))

  do_eval <<- FALSE;
  strict <<- FALSE;
  failing_context_notification_id <<- NULL

  server <- function (input, output, session) {
    invalidatePeriodically <- reactiveTimer(intervalMs = 1000);

    old_path <- "";
    old_hash <- "";

    observe({
      invalidatePeriodically();

      tryCatch(
        expr = {
          c(id, path, textContents, selections) %<-% rstudioapi::getSourceEditorContext()[0:4];
          if (!is.null(failing_context_notification_id)) {
            removeNotification(failing_context_notification_id)
            failing_context_notification_id <<- NULL
          }

          file_name <- tail(strsplit(path, "/")[[1]], n = 1);
          hash <- digest::digest(textContents, "md5");

          if (path != old_path || hash != old_hash) {

            tryCatch(
              expr = withCallingHandlers(
                expr = {
                  withProgress(
                    expr = c(variables, functions, hypotheses) %<-% loop(textContents, interactive = TRUE),
                    min = 1,
                    max = length(textContents),
                    message = paste0(c("Loading", file_name), collapse = " ")
                  )

                  output$graph <- renderRDataFlow({
                    RDataFlow(list(
                      variables = variables,
                      functions = unname(apply(functions, 1, as.list)),
                      hypotheses = unname(apply(hypotheses, 1, as.list))
                    ))
                  });
                },
                warning = function (w) {
                  action = NULL

                  if (grepl("there is no package", w$message)) {
                    lib = regmatches(w$message, regexec("package called ‘(\\w+)’", w$message, perl = TRUE))[[1]][2]

                    action = tags$a(href='',
                                    onclick=paste0(c('event.preventDefault(); Shiny.setInputValue("install", "', lib, '");'), collapse = ""),
                                    paste0(c("Install the `", lib, "` package"), collapse = ""))
                  }

                  showNotification(w$message, action = action, type = "warning")
                }
              ),

              error = function (e) {
                action = NULL;

                if (grepl("there is no package", e$message)) {
                  lib = regmatches(e$message, regexec("package called ‘(\\w+)’", e$message, perl = TRUE))[[1]][2]

                  action = tags$a(href='',
                                  onclick=paste0(c('event.preventDefault(); Shiny.setInputValue("install", "', lib, '");'), collapse = ""),
                                  paste0(c("Install the `", lib, "` package"), collapse = ""))

                } else if ("custom_code" %in% names(e)) {
                  action = tags$a(href='',
                                  onclick=paste0(c('event.preventDefault(); Shiny.setInputValue("comment", ', jsonlite::toJSON(e$lines), ');'), collapse = ""),
                                  paste0(c("Comment the expression on lines ", e$lines[1], ":", e$lines[2]), collapse = ""))

                } else if ("lines" %in% names(e)) {
                  # TODO: split into the debug mode with goto action towards the faulting code and production mode with complete masking of internal problems?

                  e$message = paste0(c("Addin has failed to parse the file ", file_name, " \nbecause of lines ", e$lines[1], ":", e$lines[2],
                                       " triggering the following exception:\n\"", e$message, "\""), collapse = "")
                }

                showNotification(e$message, action = action, type = "error", duration = NA)
              },

              finally = {
                old_path <<- path;
                old_hash <<- hash;
              }
            );
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
      );
    })

    observeEvent(
      input$goto,
      {
        c(line_no1, line_no2) %<-% lapply(input$goto, as.integer);

        rstudioapi::setSelectionRanges(
          list(rstudioapi::document_range(c(line_no1, 0), c(line_no2, Inf)))
        )
      }
    )

    observeEvent(
      input$comment,
      {
        c(line_no1, line_no2) %<-% lapply(input$comment, as.integer);

        rstudioapi::insertText(
          lapply(line_no1:line_no2, function (line) rstudioapi::document_position(line, -1L)),
          "# "
        )

        rstudioapi::setCursorPosition(rstudioapi::document_position(line_no1, -1L))
      }
    )

    observeEvent(
      input$set_strict,
      {
        strict <<- input$set_strict;
      }
    )

    observeEvent(
      input$help,
      {
        c(package, func_name) %<-% input$help;

        print(help(func_name, package = tail(strsplit(package, ":")[[1]], n = 1)))
      }
    )

    observeEvent(
      input$install,
      {
        tryCatch(expr = {
          lib <- input$install

          install.packages(lib)

          showNotification(paste0(c("Successfully installed `", lib, "`"), collapse =""))

          library(lib, character.only = TRUE)

          old_hash <<- ""
        },
        warning = function (w) showNotification(w$message, type = "warning"),
        error = function (e) showNotification(e$message, type = "error"))
      }
    )
  }

  viewer <- paneViewer(300);

  runGadget(ui, server, viewer = viewer);
}

