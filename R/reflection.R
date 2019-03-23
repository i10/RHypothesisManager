own_error <- function (message, custom_code, call = sys.call(-1), ...) {
  structure(
    class = c("own_error", "error", "condition"),
    list(message = message, call = call, custom_code = custom_code, ...)
  )
}


parse <- function (text, line_no1 = 1, line_non = NULL,
                   variables = NULL, functions = NULL, hypotheses = NULL,
                   interactive = FALSE) {
  # Prepare
  if (is.null(variables)) {
    variables <- data.frame(matrix(ncol = 8, nrow = 0));
    colnames(variables) <- c("id", "name", "precursors", "columns", "origin", "type", "value", "generation");
  }

  if (is.null(functions)) {
    functions <- data.frame(matrix(ncol = 8, nrow = 0));
    colnames(functions) <- c("id", "name", "lines", "signature", "packages", "arguments", "depth", "breakpoint");
  }

  if (is.null(hypotheses)) {
    hypotheses <- data.frame(matrix(ncol = 6, nrow = 0));
    colnames(hypotheses) <- c("id", "name", "columns", "functions", "models", "formulas");
  }

  env <<- new.env()

  # Run loop
  if (is.null(line_non))
    line_non <- length(text)

  while (line_no1 < line_non + 1) {
    line_no2 <- line_no1;
    break_ <- FALSE;

    while (!break_ && line_no2 < line_non + 1) {
      tryCatch(
        expr = {
          line <- paste0(text[line_no1:line_no2], collapse="\n");

          row <- base::parse(text=line);

          if (!is.language(row) || length(row) == 0) {
            break;
          }

          for (exp in row) {
            before_funcs <- nrow(functions);

            c(variables, functions, hypotheses) %<-% recursion(exp, variables, functions, hypotheses)

            if (nrow(functions) > before_funcs) {
              functions[(before_funcs + 1):nrow(functions), ]$lines <- list(list(line_no1, line_no2));
              functions[nrow(functions), ]$signature <- line;
            }
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
          if (grepl("unexpected end of input", e$message) ||
              grepl("INCOMPLETE_STRING", e$message)) {
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


diff <- function (text, old_text, vv, ff, hh) {
  text_diff <- ses(old_text, text)

  for (diff in text_diff) {
    # Introspect the diff data
    diff_type <- regmatches(diff, regexpr("[acd]", diff))

    diff_pair <- lapply(
      strsplit(diff, "[acd]"),
      function (line) lapply(
        line,
        function (part) as.integer(if (grepl(",", part)) strsplit(part, ",")[[1]] else c(part, part))
      )
    )[[1]]

    c(first_line, last_line) %<-% diff_pair[[1]]
    c(new_first_line, new_last_line) %<-% diff_pair[[2]]

    diff_size <- (new_last_line - new_first_line + (diff_type == "a")) - (last_line - first_line + (diff_type == "d"))

    # Split the functions to before/within/after the change
    prior_functions <-    subset(ff, sapply(lines, function (lines) lines[[2]]) < first_line)
    changed_functions <-  subset(ff, sapply(lines, function (lines) first_line <= lines[[2]] && lines[[1]] <= last_line))
    later_functions <-    subset(ff, sapply(lines, function (lines) lines[[1]]) > last_line)

    prior_variables <-    subset(vv,              type == "column" | origin %in% prior_functions$id)
    prior_variables <-    subset(prior_variables, type != "column" | id %in% unlist(prior_variables$columns))
    changed_variables <-  subset(vv,              origin %in% changed_functions$id | id %in% changed_functions$breakpoint)

    prior_hypotheses <- hh

    prior_hypotheses$functions <- lapply(prior_hypotheses$functions,  function (f) if (length(f)) as.list(intersect(f, prior_functions$id)) else f)
    prior_hypotheses$models <-    lapply(prior_hypotheses$models,     function (m) if (length(m)) as.list(intersect(m, prior_variables$id)) else m)
    prior_hypotheses$formulas <-  lapply(prior_hypotheses$formulas,   function (f) if (length(f)) as.list(intersect(f, prior_variables$id)) else f)

    prior_hypotheses <- subset(prior_hypotheses, lapply(prior_hypotheses$functions, length) > 0)

    # Process the changed content
    if (nrow(changed_functions)) {
      first_line <- changed_functions[1, ]$lines[[1]][[1]]

      new_first_line <- new_first_line - first_line + changed_functions[1, ]$lines[[1]][[1]]
      new_last_line <-  new_last_line - last_line + changed_functions[nrow(changed_functions), ]$lines[[1]][[2]]
    }

    later_functions$lines <- lapply(later_functions$lines, function (f) as.list(unlist(f) + diff_size))

    withProgress(
      expr = c(new_variables_, new_functions, new_hypotheses_) %<-% parse(text, new_first_line, new_last_line,
                                                                          interactive = TRUE,
                                                                          variables=prior_variables, hypotheses=hh),
      min = new_first_line,
      max = new_last_line,
      message = "Refreshing"
    )

    new_variables <-  subset(new_variables_,  origin %in% new_functions$id | id %in% new_functions$breakpoint)
    new_hypotheses <- subset(new_hypotheses_, sapply(new_hypotheses_$functions, function (f) length(intersect(f, new_functions$id))) > 0)

    # Check for breakpoints, new variables or TODO: other things
    old_code_had_breakpoints <- nrow(changed_variables) > 0
    new_code_has_breakpoints <- nrow(new_variables) > 0

    if (old_code_had_breakpoints || new_code_has_breakpoints) {
      return(list(prior_variables, prior_functions, prior_hypotheses, TRUE, first_line))
    }

    ff <- rbind(prior_functions, new_functions, later_functions)

    # If no acutal functions have been affected, just continue onward
    if (!nrow(changed_functions) && !nrow(new_functions))
      next

    hh$functions <- lapply(hh$functions, function (f) if (length(f)) as.list(intersect(f, ff$id)) else f)
    hh <- subset(hh, lapply(hh$functions, length) > 0)

    # NB: Variables don't need updating unless more sophisticated checks are implemented
    # vv <- vv

    if (nrow(new_hypotheses)) {
      processor <- function (h) {
        old_h <- as.list(hh[hh$id == h$id, ])

        h$functions <- as.list(union(h$functions,  unlist(old_h$functions)))
        h$models <-    as.list(union(h$models,     unlist(old_h$models)))
        h$formulas <-  as.list(union(h$formulas,   unlist(old_h$formulas)))

        h
      }

      new_hypotheses <-
        apply(new_hypotheses, 1, processor) %>%
        do.call(rbind, .) %>%
        as.data.frame

      for (id in new_hypotheses$id)
        if (any(hh$id == id))
          hh[hh$id == id, ] <- new_hypotheses[new_hypotheses$id == id, ]

      hh <- rbind(hh, subset(new_hypotheses, !id %in% hh$id))
    }
  }

  list(vv, ff, hh, FALSE, first_line)
}



find_hypothesis <- function (exp, hypotheses, variables, add = FALSE) {
  functions <- data.frame(matrix(ncol = 8, nrow = 0));
  colnames(functions) <- c("id", "name", "lines", "signature", "packages", "arguments", "depth", "breakpoint");

  c(variables, functions, h) %<-% recursion(exp[[length(exp)]], variables, functions, data.frame(),
                                            addition_mode = TRUE)

  if (nrow(functions)) {
    con_column_ids <- lapply(collect_columns(variables, functions),
                             function (column) column$id)

  } else if (is.name(exp[[length(exp)]])) {
    c(con_column_index, variables) %<-% find_variable(as.character(exp[[length(exp)]]), variables,
                                                      add = TRUE,
                                                      type_constraint = c("column", "constant"),
                                                      check_shadowing = FALSE)

    con_column_ids <- list(variables[con_column_index, ]$id)
  }

  if (length(exp) == 3) {
    # TODO: detect lsmeans/emmeans contrast functions?
    name <- paste0(as.character(exp)[c(2, 1, 3)], collapse = ' ');

    if (is.name(exp[[2]])) {
      c(dep_column_index, variables) %<-% find_variable(as.character(exp[[2]]), variables,
                                                        add = TRUE,
                                                        type_constraint = c("column", "constant"),
                                                        check_shadowing = FALSE);

      dep_column_id <- variables[dep_column_index, ]$id;

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

  for (var_index in 1:nrow(variables)) {
    variable <- as.list(variables[var_index, ]);

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
  index <- nrow(variables) + 1;

  if (!force) {
    packages <- find(name);

    # If the name can be found in the packages
    is_function_name <- !!length(packages);

    if (is_function_name && eval_) {
      # If the name can _only_ be found in the packages
      is_function_name <- !length(setdiff(grep("package:", packages), 1:length(packages)));
    }

    if (nrow(variables) > 0)
      for (i in 1:nrow(variables)) {
        old_var <- as.list(variables[i, ]);

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

  if (index > nrow(variables)) {
    if (!add) {
      e <- own_error(paste0(c("The variable `", name, "` has never occured before, but addition has not been granted"), collapse=""),
                     custom_code = 1)
      stop(e)
    }
    var <- list(
      id =          paste0(c("v", UUIDgenerate()), collapse = "-"),
      name =        name,
      precursors =  list(list()),
      columns =     list(list()),
      origin =      NA,
      type =        "constant",
      value =       NA,
      generation =  0
    );

    variables[index, ] <- var;
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

          if (variables[var_index, ]$type == "constant" && !is.na(variables[var_index, ]$value)) {
            func$arguments <- append(func$arguments, variables[var_index, ]$value);  # value is (supposed to be) atomic

          } else if (variables[var_index, ]$type == "formula") {
            func$arguments <- append(func$arguments, variables[var_index, ]$value);  # value is a hypothesis id

          } else {
            func$arguments <- append(func$arguments, variables[var_index, ]$id)
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

  do_force_add <- !length(variables[var_index, ]$columns[[1]]);

  # col2
  c(col_index, variables) %<-% find_variable(col2_name, variables,
                                             add = TRUE,
                                             force = do_force_add,
                                             type_constraint = "column",
                                             check_shadowing = FALSE);

  variables[col_index, ]$type <- "column";

  if (variables[col_index, ]$generation == 0) {
    variables[col_index, ]$generation <- variables[var_index, ]$generation + 1;
  }

  if (!(variables[col_index, ]$id %in% variables[var_index, ]$columns[[1]])) {
    variables[var_index, ]$columns[[1]] <- append(variables[var_index, ]$columns[[1]], variables[col_index, ]$id);
  }

  # data$col1 == value
  before_funcs <- nrow(functions);

  c(variables, f, hypotheses) %<-% recursion(lookup, variables, functions, hypotheses,
                                             addition_mode = addition_mode, depth = depth);

  columns <- list()

  for (func_args in f[(before_funcs + 1):nrow(f), ]$arguments) {
    for (arg in func_args) {
      for (var_index_ in 1:nrow(variables)) {
        var <- as.list(variables[var_index_, ]);

        if (var$id == arg && var$type == "column") {
          columns[[length(columns) + 1]] <- var;
          break;
        }
      }
    }
  }

  formula <- paste0(
    c(
      variables[col_index, ]$name,
      '~',
      paste0(
        lapply(columns, function (var) var$name),
        collapse = ' + '
      )
    ),
    collapse = ' '
  );

  formula <- base::parse(text=formula)[[1]];

  c(hyp_index, hypotheses, variables) %<-% find_hypothesis(formula, hypotheses, variables,
                                                           add = TRUE);

  func <- list(
    id =          paste0(c("f", UUIDgenerate()), collapse = "-"),
    name =        paste0(as.character(exp)[c(2, 1, 3)], collapse = ""),
    lines =       NA,
    signature =   NA,
    packages =    NA,
    arguments =   list(append(list(variables[var_index, ]$id, variables[col_index, ]$id), lapply(columns, function (var) var$id))),
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

    if (nrow(variables)) {
      for (var_index in 1:nrow(variables)) {
        variable <- as.list(variables[var_index, ]);

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
    }

    # Get or create the variable
    c(var_index, variables) %<-% find_variable(var_name, variables,
                                               add = TRUE,
                                               force = !is_mutation,
                                               type_constraint = c("data", "model", "constant"));

    if (is.na(variables[var_index, ]$origin)) {
      variables[var_index, ]$origin <- functions[nrow(functions), ]$id;
    }

    if (is_mutation) {
      functions[nrow(functions), ]$breakpoint <- variables[var_index, ]$id;

    } else {
      variables[var_index, ]$generation <- var_geneneration;
      variables[var_index, ]$precursors <- list(precursor_variable_ids);
    }

    # If evaluation is disabled and we can't confirm that the assigned value is a dataframe,
    #   we can still meaningfully assume that the variable is a dataframe if it is actually declared as one
    #   or infer it from the fact it has been read from the file source.
    # If evaluation is enabled, the data type will be set upon the evaluation later on
    if (!eval_ &&
        functions[nrow(functions), ]$name %in% list("subset", "[", "data.frame", "as.data.frame", "table", "rbind", "cbind") ||
        startsWith(functions[nrow(functions), ]$name, "read")) {
      variables[var_index, ]$type <- "data";

    # Explicit formula variable declarations
    } else if (before_funcs - nrow(functions) == 1 &&
               functions[nrow(functions), ]$name == "~") {
      selector <- apply(hypotheses, 1, function (hyp) functions[nrow(functions)]$id %in% hyp$functions);

      variables[var_index, ]$value <- hypotheses[selector, ]$id;
      variables[var_index, ]$type <- "formula";

      hypotheses[selector, ]$formulas[[1]] = append(hypotheses[selector, ]$formulas[[1]], variables[var_index, ]$id);

    } else if (nrow(hypotheses)) {
      for (func_id in functions[(before_funcs + 1):nrow(functions), ]$id) {
        func_args <- functions[functions$id == func_id, ]$arguments[[1]];

        selector <- apply(hypotheses, 1, function (hyp) func_id %in% hyp$functions || hyp$id %in% func_args);

        if (nrow(hypotheses[selector, ])) {
          variables[var_index, ]$type <- "model";

          for (hyp_id in hypotheses[selector, ]$id) {
            hypotheses[hypotheses$id == hyp_id, ]$models[[1]] <- append(hypotheses[hypotheses$id == hyp_id, ]$models[[1]],
                                                                        variables[var_index, ]$id)
          }
        }
      }
    }

    # 2nd (and further) gen models
    model_precursor_selector <- apply(variables, 1, function (v) v$id %in% precursor_variable_ids) & variables$type == "model"

    if (any(model_precursor_selector)) {
      parent_model_ids <- variables[model_precursor_selector, ]$id

      for (m_id in parent_model_ids) {
        hypothesis_selector <- apply(hypotheses, 1, function (h) m_id %in% h$models)
        hypotheses[hypothesis_selector, ]$models[[1]] <- append(hypotheses[hypothesis_selector, ]$models[[1]], variables[var_index, ]$id)
      }

      variables[var_index, ]$type <- "model"
    }

    if (eval_) {
      if (!is_mutation) {
        evaluation <- eval(exp[[3]], envir = env)

        if (is.data.frame(evaluation))
          variables[var_index, ]$type <- "data"

        # S4 objects cause errors when being asigned to the cell of the dataframe
        if (isS4(evaluation))
          evaluation <- NA

        else if ('package:ggplot2' %in% search() && is.ggplot(evaluation)) {
          variables[var_index, ]$type <- "model"  # TODO: replace with an exclusive data type
          evaluation <- NA

        # Wrap the collections so that they don't cripple the main dataframe
        } else if (is.vector(evaluation) || is.matrix(evaluation) || is.list(evaluation) || is.data.frame(evaluation))
          evaluation <- list(evaluation)

        else if (is.null(evaluation))
          evaluation <- NA

        variables[var_index, ]$value <- evaluation
      }

      eval(exp, envir = env)
    }

    if (variables[var_index, ]$type == "data" && !is.na(variables[var_index, ]$value) && !length(variables[var_index, ]$columns[[1]])) {
      for (col_name in colnames(variables[var_index, ]$value[[1]])) {
        c(col_index, variables) %<-% find_variable(col_name, variables,
                                                   add = TRUE,
                                                   force = TRUE,
                                                   check_shadowing = FALSE)

        variables[col_index, ]$type <- "column"
        variables[col_index, ]$generation <- variables[var_index, ]$generation + 1
        variables[var_index, ]$columns[[1]] <- append(variables[var_index, ]$columns[[1]], variables[col_index, ]$id)
      }
    }

  # Is "[" call
  } else if (is.call(exp) && identical(exp[[1]], quote(`[`)) && !force_as_function) {
    # data[data$col1 == value, "col2"]
    if (length(exp) == 4) {
      col_name <- exp[[4]];
      lookup <- exp[[3]];

      # data[data$col1 == value, ] <- but w/o the $ in the end
      # data[, smth] <- if smth is not a name of the column
      if (missing(col_name) || (missing(lookup) && !is.atomic(col_name))) {
        c(variables, functions, hypotheses) %<-% recursion(exp, variables, functions, hypotheses,
                                                           addition_mode = addition_mode, depth = depth,
                                                           force_as_function = TRUE);

      # data[, "col"]
      } else if (missing(lookup) && is.atomic(col_name)) {
        exp[[1]] <- quote(`$`)
        exp[[3]] <- NULL
        c(variables, functions, hypotheses) %<-% recursion(exp, variables, functions, hypotheses,
                                                           addition_mode = addition_mode, depth = depth - 1);

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
                                                 force = !length(variables[var_index, ]$columns[[1]]),
                                                 type_constraint = "column",
                                                 check_shadowing = FALSE);

      variables[col_index, ]$generation <- variables[var_index, ]$generation + 1;
      variables[col_index, ]$type <- "column";

      if (!(variables[col_index, ]$id %in% variables[var_index, ]$columns[[1]])) {
        variables[var_index, ]$columns[[1]] <- append(variables[var_index, ]$columns[[1]], variables[col_index, ]$id)
      }

      func <- list(
        id =          paste0(c("f", UUIDgenerate()), collapse = "-"),
        name =        paste0(as.character(exp)[c(2, 1, 3)], collapse = ""),
        lines =       NA,
        signature =   NA,
        packages =    NA,
        arguments =   list(list(variables[var_index, ]$id, variables[col_index, ]$id)),
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
    eval(exp, envir = env);

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

      # If in the "shallow" mode and any of the variables referenced in the hypothesis are not "columns"
      #   -- brand them as such and add them to whichever "dataset" variable found within arguments of the same function
      if (!eval_ && nrow(hypotheses)) {
        hypothesis_selector <- apply(hypotheses, 1, function (h) func$id %in% h$functions || h$id %in% func$arguments)

        if (nrow(hypotheses[hypothesis_selector, ])) {
          columns <- hypotheses[hypothesis_selector, ]$columns[[1]]

          column_list <- columns$control

          if (!is.null(columns$dependant))
            column_list <- append(column_list, columns$dependant)

          constant_selector <- variables$type != "column" & apply(variables, 1, function (v) v$id %in% column_list)
          dataset_selector <- variables$type == "data" & apply(variables, 1, function (v) v$id %in% func$arguments)

          if (any(constant_selector) && nrow(variables[dataset_selector, ])) {
            data_var <- as.list(variables[dataset_selector, ])

            for (col_id in column_list)
              if (!(col_id %in% data_var$columns)) {
                variables[variables$id == col_id, ]$type <- "column"
                data_var$columns[[1]] <- append(data_var$columns[[1]], col_id)
              }
          }
        }
      }

      # TODO: improve the hypothesis selection
      if (nrow(hypotheses)) {
        col_ids <- list();

        for (var_index in 1:nrow(variables)) {
          var <- as.list(variables[var_index, ]);

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
    dataset_name <- variables[apply(variables, 1, function (v) id %in% v$columns ), ]$name

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
  library(miniUI);
  library(rstudioapi);
  library(shiny);

  options(stringsAsFactors = FALSE);

  eval_ <<- FALSE;
  strict <<- FALSE;
  pause <<- FALSE;

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
    invalidatePeriodically <- reactiveTimer(intervalMs = 1000);

    old_path <- ""
    old_hash <- ""
    old_text <- ""

    variables <- NULL
    functions <- NULL
    hypotheses <- NULL

    observe({
      invalidatePeriodically();

      if (pause)
        return()

      tryCatch(
        expr = {
          c(id, path, textContents, selections) %<-% getSourceEditorContext()[0:4];

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
                    c(vv, ff, hh, too_many_changes, first_line) %<-% diff(textContents, old_text, variables, functions, hypotheses)
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
                  dumpability_selector = apply(vv, 1, function (v) is.na(v$value) || tryCatch(expr = { jsonlite::toJSON(v); TRUE }, error = function(...) FALSE))

                  if (!all(dumpability_selector))
                    vv[!dumpability_selector, ]$value <- NA

                  output$graph <- renderHypothesisManager({
                    HypothesisManager(list(
                      variables = unname(apply(vv, 1, as.list)),
                      functions = unname(apply(ff, 1, as.list)),
                      hypotheses = unname(apply(hh, 1, as.list))
                    ))
                  });
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
                action = NULL;

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

        setCursorPosition(document_position(line_no1, 0))

        setSelectionRanges(
          list(document_range(c(line_no1, 0), c(line_no2, Inf)))
        )
      }
    )

    observeEvent(
      input$select,
      {
        selector <- apply(functions, 1, function (f) f$id %in% input$select)

        selection_ranges <- lapply(
          functions[selector, ]$lines,
          function (lines) document_range(c(lines[[1]], 0), c(lines[[2]], Inf))
        )

        setSelectionRanges(selection_ranges)
      }
    )

    observeEvent(
      input$comment,
      {
        c(line_no1, line_no2) %<-% lapply(input$comment, as.integer);

        insertText(
          lapply(line_no1:line_no2, function (line) document_position(line, -1L)),
          "# "
        )

        setCursorPosition(document_position(line_no1, -1L))

        removeNotification(last_error_id)
      }
    )

    observeEvent(input$exit,    { stopApp() })

    observeEvent(input$execute, {
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
    })

    observeEvent(input$strict,  { strict <<- input$strict })

    observeEvent(input$pause,   { pause <<- input$pause })

    observeEvent(input$eval, {
      if (input$eval == eval_) {
        eval_ <<- !input$eval

        old_hash <<- ""
      }
    })

    observeEvent(
      input$help,
      {
        c(package, func_name) %<-% input$help;

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

        function_selector <- apply(functions, 1, function(f) f$id %in% hypotheses[hypotheses$id == input$edit_hypothesis, ]$functions[[1]])

        t <- FALSE
        for (i in 1:nrow(functions)) {
          if (function_selector[i] && functions[i, ]$depth > 1) {
            t <- TRUE
            function_selector[i] <- FALSE

          } else if (t && functions[i, ]$depth == 1) {
            function_selector[i] <- TRUE;
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
        showModal(hypothesisEditor(
          hypothesis = as.list(hypotheses[hypotheses$id == input$copy_hypothesis, ]),
          title = "Edit",
          action_id = "replace_selection",
          variables = variables
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

        function_selector <- apply(functions, 1, function (f) f$id %in% input$select)

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
      input$copy,
      {
        showModal(hypothesisEditor(
          hypothesis = as.list(hypotheses[hypotheses$id == input$copy_hypothesis, ]),
          title = "Copy",
          action_id = "paste",
          variables = variables
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

        function_selector <- apply(functions, 1, function (f) f$id %in% input$select)

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

  viewer <- paneViewer(300);

  runGadget(ui, server, viewer = viewer);
}

