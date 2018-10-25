library(miniUI);
#library(rstudioapi);
library(shiny);
library(uuid);

unwrapSelections <- function (selection) {
  return(selection$text);
}


is_variable <- function(x) {
  return(is.name(x) && !identical(x, quote(`<-`)));
}


loop <- function (text) {
  options(stringsAsFactors = FALSE);

  exp <- parse(text=text);

  variables <- list();

  functions <- data.frame(matrix(ncol = 5, nrow = 0));
  colnames(functions) <- c("id", "name", "arguments", "depth", "breakpoint");

  hypotheses <- data.frame(matrix(ncol = 5, nrow = 0));
  colnames(hypotheses) <- c("id", "name", "columns", "functions", "models");

  for (row in exp) {
    if (!is.language(row))
      next;

    c(variables, functions, hypotheses) %<-% recursion(row, variables, functions, hypotheses)
  }

  return(list(variables, functions, hypotheses));
}


find_hypothesis <- function (exp, hypotheses, add = FALSE) {
  if (length(exp) == 3) {
    name <- paste0(as.character(exp)[c(2, 1, 3)], collapse = ' ');
    c(columns, f, h) %<-% recursion(exp[[3]], list(), data.frame(matrix(ncol = 5, nrow = 0)), data.frame(),
                                    lookup_mode = TRUE);

    columns = list(
      dependant = as.character(exp[[2]]),
      control = lapply(columns, function(column) {return(column$name)})
    )

  } else if (length(exp) == 2) {
    name <- paste0(exp, collapse = '');

    columns = list(
      dependant = NULL,
      control = list(as.character(exp[[2]]))
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
      models =    list(list())
    );

    hypotheses[index,] <- hypothesis;
  }

  return(list(index, hypotheses))
}


update_hypothesis <- function (hypothesis, variables) {
  for (var in variables) {
    hypothesis$columns[[1]]$control <- replace(hypothesis$columns[[1]]$control,
                                               hypothesis$columns[[1]]$control == var$name,
                                               var$id)

    if (!is.null(hypothesis$columns[[1]]$dependant) && hypothesis$columns[[1]]$dependant == var$name) {
      hypothesis$columns[[1]]$dependant <- var$id;
    }
  }

  return (hypothesis);
}


find_variable <- function (name, variables, add = FALSE, force = FALSE, type_constraint = NULL) {
  var <- NULL;
  index <- length(variables) + 1;

  if (length(variables) > 0 && !force)
    for (i in 1:length(variables)) {
      old_var <- variables[[i]];

      if (old_var$name == name && (is.null(type_constraint) || any(old_var$type == type_constraint))) {
        var <- old_var;
        index <- i;
      }
    }

  if (index > length(variables) && add) {
    var <- list(
      id =          paste0(c("v", UUIDgenerate()), collapse = "-"),
      name =        name,
      precursors =  list(),
      columns =     list(),
      origin =      NULL,
      type =        "data"
    );

    variables[[index]] <- var;
  }

  return(list(index, variables))
}

argument_recursion <- function (args, func,
                                variables, functions, hypotheses,
                                depth, assignment_mode = FALSE) {
  for (arg in args) {
    if (missing(arg)) {

    } else if (is.atomic(arg)) {
      func$arguments <- append(func$arguments, arg)

    } else if (is.name(arg)) {
      c(var_index, variables) %<-% find_variable(as.character(arg), variables,
                                                 add = assignment_mode);

      if (var_index <= length(variables)) {
        # variables[[var_index]]$functions = append(variables[[var_index]]$functions, func$id)

        func$arguments <- append(func$arguments, variables[[var_index]]$id)
      }

    } else if (is.call(arg) && identical(arg[[1]], quote(`~`))) {
      c(hyp_index, hypotheses) %<-% find_hypothesis(arg, hypotheses, add = TRUE);

      hypotheses[hyp_index,] <- update_hypothesis(as.list(hypotheses[hyp_index,]), variables);

      func$arguments <- append(func$arguments, hypotheses[hyp_index,]$id);
      hypotheses[hyp_index,]$functions[[1]] = append(hypotheses[hyp_index,]$functions[[1]], func$id);

    } else if (is.call(arg) && (identical(arg[[1]], quote(`c`)) || identical(arg[[1]], quote(`list`)))) {
      c(func, variables, functions, hypotheses) %<-% argument_recursion(as.list(arg)[2:length(arg)], func,
                                                                        variables, functions, hypotheses,
                                                                        depth, assignment_mode = assignment_mode)

    } else {
      before_funcs <- nrow(functions);

      c(variables, functions, hypotheses) %<-% recursion(arg, variables, functions, hypotheses,
                                                         assignment_mode = assignment_mode, depth = depth);

      if (nrow(functions) != before_funcs) {
        func$arguments <- append(func$arguments, functions[nrow(functions), ]$id);
      }
    }
  }

  return(list(func, variables, functions, hypotheses))
}


recursion <- function (exp, variables, functions, hypotheses,
                       assignment_mode = FALSE, lookup_mode = FALSE,
                       depth = 0) {
  depth <- depth + 1;

  # Is assignment line
  if (is.call(exp) && (identical(exp[[1]], quote(`<-`)) || identical(exp[[1]], quote(`=`)))) {
    if (is.call(exp[[2]])) {
      c(variables, tmp, hypotheses) %<-% recursion(exp[[2]], variables, functions, hypotheses,
                                                   assignment_mode = TRUE, depth = depth)

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
                                                       assignment_mode = TRUE, depth = depth);

    is_mutation <- FALSE;
    precursor_variable_ids <- list();

    for (variable in variables) {
      for (func_args in functions[(before_funcs + 1):nrow(functions), ]$arguments) {
        if (variable$id %in% func_args) {
          if (variable$type == "column") {
            # TODO: do smth?

          } else if (variable$name == var_name) {
            is_mutation <- TRUE;

          } else if (!(variable$id %in% precursor_variable_ids)) {
            precursor_variable_ids <- append(precursor_variable_ids, variable$id);
          }
        }
      }
    }

    # Get or create the variable
    c(var_index, variables) %<-% find_variable(var_name, variables, add = TRUE, force = !is_mutation,
                                               type_constraint = c("data", "model", "constant"));

    if (is.null(variables[[var_index]]$origin)) {
      variables[[var_index]]$origin <- functions[nrow(functions), ]$id;
    }

    if (is_mutation) {
      functions[nrow(functions), ]$breakpoint <- variables[[var_index]]$id;
    }

    variables[[var_index]]$precursors <- precursor_variable_ids;

    if (nrow(hypotheses)) {
      for (func_id in functions[(before_funcs + 1):nrow(functions), ]$id) {
        selector <- apply(hypotheses, 1, function (hyp) { return(func_id %in% hyp$functions); });

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

  # # Is "[" call
  # } else if (is.call(exp) && identical(exp[[1]], quote(`[`))) {
  #   c(variables, functions, hypotheses) %<-% recursion(exp[[2]], variables, functions, hypotheses,
  #                                                      assignment_mode = assignment_mode, depth = depth);
  #
  # Is "$" call
  } else if (is.call(exp) && identical(exp[[1]], quote(`$`))) {
    if (is.call(exp[[2]])) {
      # data[data$col1 == value, ]$col2
      if (identical(exp[[2]][[1]], quote(`[`))) {
        # data
        c(var_index, variables) %<-% find_variable(as.character(exp[[2]][[2]]), variables,
                                                   add = TRUE,
                                                   type_constraint = "data");

        do_force_add <- !length(variables[[var_index]]$columns);

        # col2
        c(col_index, variables) %<-% find_variable(as.character(exp[[3]]), variables,
                                                   add = TRUE,
                                                   force = do_force_add,
                                                   type_constraint = "column");

        variables[[col_index]]$type <- "column";

        if (!(variables[[col_index]]$id %in% variables[[var_index]]$columns)) {
          variables[[var_index]]$columns <- append(variables[[var_index]]$columns, variables[[col_index]]$id);
        }

        # data$col1 == value
        before_funcs <- nrow(functions);

        c(variables, f, hypotheses) %<-% recursion(exp[[2]][[3]], variables, functions, hypotheses,
                                                   assignment_mode = assignment_mode, lookup_mode = lookup_mode, depth = depth);

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
              lapply(columns, function(var) { return(var$name); }),
              collapse = ' + '
            )
          ),
          collapse = ' '
        );

        c(hyp_index, hypotheses) %<-% find_hypothesis(parse(text = formula)[[1]], hypotheses,
                                                      add = TRUE);

        hypotheses[hyp_index,] <- update_hypothesis(as.list(hypotheses[hyp_index,]), variables);

        func <- list(
          id =          paste0(c("f", UUIDgenerate()), collapse = "-"),
          name =        paste0(as.character(exp)[c(2, 1, 3)], collapse = ""),
          arguments =   list(append(list(variables[[var_index]]$id, variables[[col_index]]$id), lapply(columns, function(var) { return(var$id); }))),
          depth =       depth - assignment_mode,
          breakpoint =  NA
        );

        hypotheses[hyp_index, ]$functions[[1]] <- append(hypotheses[hyp_index, ]$functions[[1]], func$id);

        functions[nrow(functions) + 1, ] <- func;

      # func()$column
      } else {
        c(variables, functions, hypotheses) %<-% recursion(exp[[2]], variables, functions, hypotheses,
                                                           assignment_mode = assignment_mode, depth = depth - 1);
      }

    # data$column
    } else {
      c(var_index, variables) %<-% find_variable(as.character(exp[[2]]), variables,
                                                 add = assignment_mode || lookup_mode,
                                                 type_constraint = c("data", "model", "constant"));

      c(col_index, variables) %<-% find_variable(as.character(exp[[3]]), variables,
                                                 add = TRUE,
                                                 force = !length(variables[[var_index]]$columns),
                                                 type_constraint = "column");

      variables[[col_index]]$type <- "column";

      if (!(variables[[col_index]]$id %in% variables[[var_index]]$columns)) {
        variables[[var_index]]$columns <- append(variables[[var_index]]$columns, variables[[col_index]]$id)
      }

      func <- list(
        id =          paste0(c("f", UUIDgenerate()), collapse = "-"),
        name =        paste0(as.character(exp)[c(2, 1, 3)], collapse = ""),
        arguments =   list(list(variables[[var_index]]$id, variables[[col_index]]$id)),
        depth =       depth - assignment_mode,
        breakpoint =  NA
      );

      functions[nrow(functions) + 1, ] <- func;
    }

  # Is "~" call -- formula (aka hypothesis) initialization
  } else if (is.call(exp) && identical(exp[[1]], quote(`~`))) {
    c(index, hypotheses) %<-% find_hypothesis(exp, hypotheses, add = TRUE);

  # Is function declaration
  } else if (is.call(exp) && identical(exp[[1]], quote(`function`))) {
    # TODO: introduce scopes

    # functions[[length(functions) + 1]] <- exp;

  # Is library import call
  } else if (is.call(exp) && identical(exp[[1]], quote(`library`))) {
    # Skip retrievals of property

  # Is library name call
  } else if (is.call(exp) && identical(exp[[1]], quote(`::`))) {
    # Shouldn't really ever pop up

  # Is paranthesis (?!)
  } else if (is.call(exp) && identical(exp[[1]], quote(`(`))) {
    c(variables, functions, hypotheses) %<-% recursion(exp[[2]], variables, functions, hypotheses, depth = depth);

  # Is if/for clause
  } else if (is.call(exp) && (identical(exp[[1]], quote(`if`)) || identical(exp[[1]], quote(`for`)))) {
    # TODO: add conditional as the precursor?

    c(variables, functions, hypotheses) %<-% recursion(exp[[3]], variables, functions, hypotheses, depth = depth);

  # Is block statement
  } else if (is.call(exp) && identical(exp[[1]], quote(`{`))) {
    for (line in as.list(exp)[2:length(exp)])
      c(variables, functions, hypotheses) %<-% recursion(line, variables, functions, hypotheses, depth = depth);

  # Is a generic function call
  } else if (is.call(exp)) {
    if (is.call(exp[[1]])) {
      func_name <- paste0(as.character(exp[[1]])[c(2, 1, 3)], collapse = "");

    } else {
      func_name <- as.character(exp[[1]]);
    }

    func <- list(
      id =          paste0(c("f", UUIDgenerate()), collapse = "-"),
      name =        func_name,
      arguments =   list(),
      depth =       depth - assignment_mode,
      breakpoint =  NA
    );

    if (length(exp) > 1) {
      c(func, variables, functions, hypotheses) %<-% argument_recursion(as.list(exp)[2:length(exp)], func,
                                                                        variables, functions, hypotheses,
                                                                        depth = depth, assignment_mode=assignment_mode);

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

    # NB: does not add the variable to the list
    c(tmp, variables) %<-% find_variable(as.character(exp), variables, add = assignment_mode || lookup_mode);

  # Is atomic
  } else if (is.atomic(exp)) {

  # Dunno
  } else {

  }

  return(list(variables, functions, hypotheses))
}


addin <- function() {
  ui = bootstrapPage(
    tag('svg', list()),
    RDataFlowOutput("graph", '100%', '600px')
  )

  server <- function(input, output, session) {
    invalidatePeriodically <- reactiveTimer(intervalMs = 1000);
    old_path <- "";
    old_hash <- ""

    observe({
      invalidatePeriodically()

      c(id, path, textContents, selections) %<-% rstudioapi::getActiveDocumentContext()[0:4];

      hash <- digest::digest(textContents, "md5");

      if (path != old_path || hash != old_hash)
        tryCatch(
          #selections <- lapply(selections, unwrapSelections);

          expr = {
            c(variables, functions, hypotheses) %<-% loop(textContents);

            output$graph <- renderRDataFlow({
              RDataFlow(list(
                variables = variables,
                functions = unname(apply(functions, 1, as.list)),
                hypotheses = unname(apply(hypotheses, 1, as.list))
              ))
            })

            old_path <<- path;
            old_hash <<- hash;
          },
          warning = function (...) {},
          error = function(...) {},
          finally = {}
        )
    })
  }

  viewer <- paneViewer(300);

  runGadget(ui, server, viewer = viewer);
}

