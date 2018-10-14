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
  exp <- parse(text=text);

  variables <- list();
  functions <- list();
  hypotheses <- list();

  for (row in exp) {
    if (!is.language(row))
      next;

    c(variables, functions, hypotheses) %<-% recursion(row, variables, functions, hypotheses)
  }

  return(list(variables, functions, hypotheses));
}


find_hypothesis <- function (exp, hypitheses, add = FALSE) {
  hypothesis <- NULL;
  index <- length(hypitheses) + 1;

  if (length(exp) == 3) {
    name <- paste0(as.character(exp)[c(2, 1, 3)], collapse = ' ');
    columns <- recursion(exp[[3]], list(), list(), list(), lookup_mode = TRUE)[[1]]

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

  if (length(hypitheses) > 0)
    for (i in 1:length(hypitheses)) {
      old_hypothesis <- hypitheses[[i]];

      if (old_hypothesis$name == name) {
        hypothesis <- old_hypothesis;
        index <- i;

        break;
      }
    }

  if (index > length(hypitheses) && add) {
    hypothesis <- list(
      id =        UUIDgenerate(),
      name =      name,
      columns =   columns,
      functions = list(),
      models =    list()
    );

    hypitheses[[index]] <- hypothesis;
  }

  return(list(index, hypitheses))
}


update_hypothesis <- function (hypothesis, variables) {
  for (var in variables) {
    hypothesis$columns$control <- replace(hypothesis$columns$control, hypothesis$columns$control==var$name, var$id)

    if (!is.null(hypothesis$columns$dependant) && hypothesis$columns$dependant == var$name) {
      hypothesis$columns$dependant <- var$id;
    }
  }

  return (hypothesis);
}


find_variable <- function (name, variables, add = FALSE, force = FALSE) {
  var <- NULL;
  index <- length(variables) + 1;

  if (length(variables) > 0 && !force)
    for (i in 1:length(variables)) {
      old_var <- variables[[i]];

      if (old_var$name == name) {
        var <- old_var;
        index <- i;
      }
    }

  if (index > length(variables) && add) {
    var <- list(
      id =          UUIDgenerate(),
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


recursion <- function (exp, variables, functions, hypotheses,
                       assignment_mode = FALSE, lookup_mode = FALSE,
                       depth = 0) {
  depth <- depth + 1;

  # Is assignment line
  if (is.call(exp) && (identical(exp[[1]], quote(`<-`)) || identical(exp[[1]], quote(`=`)))) {
    if (is.call(exp[[2]])) {
      c(variables, tmp, hypotheses) %<-% recursion(exp[[2]], variables, list(), hypotheses,
                                                   assignment_mode = TRUE, depth = depth)

      var_name <- as.character(exp[[2]][[2]]);

    } else {
      var_name <- as.character(exp[[2]]);
    }

    # Find the precursors and invoked functions
    c(precursor_variables, mutator_functions, invoked_hypotheses) %<-% recursion(exp[[3]], list(), list(), list(),
                                                                                 assignment_mode = TRUE, depth = depth);

    # is_mutation <- length(precursor_variables[precursor_variables$name == var_name]) == 0
    is_mutation <- FALSE;

    for (pre_var in precursor_variables) {
      if (pre_var$name == var_name) {
        is_mutation <- TRUE;
        break;
      }
    }

    # Get or create the variable
    c(var_index, variables) %<-% find_variable(var_name, variables, add = TRUE, force = !is_mutation);

    new_var_id <- variables[[var_index]]$id;

    # Add the newly gathered data to the larger massif
    for (pre_var_ in precursor_variables) {
      c(pre_var_index, variables) %<-% find_variable(pre_var_$name, variables, add = TRUE);

      pre_var <- variables[[pre_var_index]];

      if (pre_var$type == "column") {
        next;
      }

      for (i in 1:length(mutator_functions)) {
        mutator <- mutator_functions[[i]];

        mutator$arguments <- replace(mutator$arguments, mutator$arguments == pre_var_$id, pre_var$id)

        if (pre_var$name == variables[[var_index]]$name) {
          mutator$breakpoint <- new_var_id;
        }

        mutator_functions[[i]] <- mutator;
      }

      if (pre_var$id != new_var_id && !(pre_var$id %in% variables[[var_index]]$precursors)) {
        variables[[var_index]]$precursors <- append(variables[[var_index]]$precursors, pre_var$id);
      }
    }

    mutator <- mutator_functions[[1]];

    if (is.null(variables[[var_index]]$origin)) {
      variables[[var_index]]$origin <- mutator$id;
    }

    # functions <- rbind(functions, mutator)
    functions[[length(functions) + 1]] <- mutator;

    for (hyp in invoked_hypotheses) {
      c(hyp_index, hypotheses) %<-% find_hypothesis(parse(text=hyp$name)[[1]], hypotheses, add = TRUE);

      hypotheses[[hyp_index]]$functions <- append(hypotheses[[hyp_index]]$functions, hyp$functions);

      variables[[var_index]]$type <- "model";
      hypotheses[[hyp_index]]$models <- append(hypotheses[[hyp_index]]$models, new_var_id);
    }

  # # Is "[" call
  # } else if (is.call(exp) && identical(exp[[1]], quote(`[`))) {
  #   c(variables, functions, hypotheses) %<-% recursion(exp[[2]], variables, functions, hypotheses,
  #                                                      assignment_mode = assignment_mode, depth = depth);
  #
  # Is "$" call
  } else if (is.call(exp) && identical(exp[[1]], quote(`$`))) {
    if (is.call(exp[[2]])) {
      # data[data$col1 == value]$col2
      if (identical(exp[[2]][[1]], quote(`[`))) {
        # data
        c(var_index, variables) %<-% find_variable(as.character(exp[[2]][[2]]), variables,
                                                   add = TRUE);

        do_force_add <- !length(variables[[var_index]]$columns);

        # col2
        c(col_index, variables) %<-% find_variable(as.character(exp[[3]]), variables,
                                                   add = TRUE,
                                                   force = do_force_add);

        variables[[col_index]]$type <- "column";

        if (!(variables[[col_index]]$id %in% variables[[var_index]]$columns)) {
          variables[[var_index]]$columns <- append(variables[[var_index]]$columns, variables[[col_index]]$id);
        }

        # data$col1 == value
        # c(variables, functions, hypotheses) %<-% recursion(exp[[2]][[3]], variables, functions, hypotheses,
        #                                                    assignment_mode = assignment_mode, depth = depth);
        c(col2_index, variables) %<-% find_variable(as.character(exp[[2]][[3]][[2]][[3]]), variables,
                                                    add = TRUE,
                                                    force <- do_force_add);

        variables[[col2_index]]$type <- "column";

        if (!(variables[[col2_index]]$id %in% variables[[var_index]]$columns)) {
          variables[[var_index]]$columns <- append(variables[[var_index]]$columns, variables[[col2_index]]$id);
        }

        formula <- paste0(c(variables[[col_index]]$name, '~', variables[[col2_index]]$name), collapse = ' ');

        c(hyp_index, hypotheses) %<-% find_hypothesis(parse(text = formula)[[1]], hypotheses,
                                                    add = TRUE);

        hypotheses[[hyp_index]] <- update_hypothesis(hypotheses[[hyp_index]], variables);

        func <- list(
          id =        UUIDgenerate(),
          name =      paste0(as.character(exp), collapse = " "),
          arguments = list(variables[[var_index]]$id, variables[[col_index]]$id, variables[[col2_index]]$id),
          depth =     depth
        );

        functions[[length(functions) + 1]] <- func;

      # func()$column
      } else {
        c(variables, functions, hypotheses) %<-% recursion(exp[[2]], variables, functions, hypotheses,
                                                           assignment_mode = assignment_mode, depth = depth - 1);
      }

    # data$column
    } else {
      c(var_index, variables) %<-% find_variable(as.character(exp[[2]]), variables,
                                                 add = assignment_mode);

      c(col_index, variables) %<-% find_variable(as.character(exp[[3]]), variables,
                                                 add = TRUE,
                                                 force = !length(variables[[var_index]]$columns));

      variables[[col_index]]$type <- "column";

      if (!(variables[[col_index]]$id %in% variables[[var_index]]$columns)) {
        variables[[var_index]]$columns <- append(variables[[var_index]]$columns, variables[[col_index]]$id)
      }

      func <- list(
        id =        UUIDgenerate(),
        name =      as.character(exp[[1]]),
        arguments = list(variables[[var_index]]$id, variables[[col_index]]$id),
        depth =     depth
      );

      functions[[length(functions) + 1]] <- func;
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
    func <- list(
      id =        UUIDgenerate(),
      name =      as.character(exp[[1]]),
      arguments = list(),
      depth =     depth - assignment_mode
    );

    if (length(exp) > 1) {
      for (arg in as.list(exp)[2:length(exp)]) {
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

          hypotheses[[hyp_index]] <- update_hypothesis(hypotheses[[hyp_index]], variables);

          func$arguments <- append(func$arguments, hypotheses[[hyp_index]]$id);
          hypotheses[[hyp_index]]$functions = append(hypotheses[[hyp_index]]$functions, func$id);

        } else {
          before_vars <- length(variables);

          c(variables, nested_functions, hypotheses) %<-% recursion(arg, variables, list(), hypotheses,
                                                                    assignment_mode = assignment_mode, depth = depth);

          if (length(nested_functions)) {
            func$arguments <- append(func$arguments, nested_functions[[1]]$arguments);
            # func$arguments <- append(func$arguments, functions[[length(functions)]]$id);

          } else if (length(variables) != before_vars) {
            func$arguments <- append(func$arguments, lapply(variables[before_vars:length(variables)], function(var) { return(var$id); }));
          }
        }
      }

      if (func$depth == 1 && length(hypotheses)) {
        col_ids <- list();

        for (var in variables) {
          if (var$id %in% func$arguments && var$type == "column") {
            col_ids <- append(col_ids, var$id);
          }
        }

        if (length(col_ids)) {
          for (hyp_index in 1:length(hypotheses)) {
            hypothesis <- hypotheses[[hyp_index]];

            columns <- append(hypothesis$columns$control, hypothesis$columns$dependant);

            if (length(setdiff(columns, col_ids)) == 0) {
              hypothesis$functions <- append(hypothesis$functions, func$id);

              hypotheses[[hyp_index]] <- hypothesis;
              break;
            }
          }
        }
      }
    }

    functions[[length(functions) + 1]] <- func;

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
                functions = functions,
                hypotheses = hypotheses
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

