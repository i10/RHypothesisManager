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
    hypothesis <- list();

    hypothesis$id <- UUIDgenerate();
    hypothesis$name <- name;
    hypothesis$columns <- columns;
    hypothesis$functions <- list();
    hypothesis$models <- list();

    hypitheses[[index]] <- hypothesis;
  }

  return(list(index, hypitheses))
}


find_variable <- function (name, variables, add = FALSE) {
  var <- NULL;
  index <- length(variables) + 1;

  if (length(variables) > 0)
    for (i in 1:length(variables)) {
      old_var <- variables[[i]];

      if (old_var$name == name) {
        var <- old_var;
        index <- i;
      }
    }

  if (index > length(variables) && add) {
    var <- list();

    var$id <- UUIDgenerate();
    var$name <- name;
    var$functions <- list();
    var$precursors <- list();

    variables[[index]] <- var;
  }

  return(list(index, variables))
}


recursion <- function (exp, variables, functions, hypotheses,
                       assignment_mode = FALSE, lookup_mode = FALSE,
                       depth = 0) {
  depth <- depth + 1;

  # Is assignment row
  if (is.call(exp) && (identical(exp[[1]], quote(`<-`)) || identical(exp[[1]], quote(`=`)))) {
    if (is.call(exp[[2]])) {
      exp[[2]] <- exp[[2]][[2]];
    }

    tmp <- length(variables)

    # Get or create the variable
    c(var_index, variables) %<-% find_variable(as.character(exp[[2]]), variables, add = TRUE);

    # Find the precursors and invoked functions
    c(precursor_variables, mutator_functions, invoked_hypotheses) %<-% recursion(exp[[3]], list(), list(), list(),
                                                                                 assignment_mode = TRUE, depth = depth);

    # if (tmp == length(variables) && length(precursor_variables[precursor_variables$name == as.character(exp[[2]])]) == 0) {
    #   var <- list();
    #
    #   var$id <- UUIDgenerate();
    #   var$name <- as.character(exp[[2]]);
    #   var$functions <- list();
    #   var$precursors <- list();
    #
    #   var_index = length(variables);
    #
    #   variables[[var_index]] <- var;
    # }

    if (tmp == length(variables)) {
      flag <- TRUE;

      for (pre_var in precursor_variables)
        if (pre_var$name == variables[[var_index]]$name) {
          flag <- FALSE;
          break;
        }

      if (flag) {
        var <- list();

        var$id <- UUIDgenerate();
        var$name <- as.character(exp[[2]]);
        var$functions <- list();
        var$precursors <- list();

        var_index = length(variables) + 1;

        variables[[var_index]] <- var;
      }
    }

    new_var_id <- variables[[var_index]]$id;

    # Add the newly gathered data to the larger massif
    for (pre_var in precursor_variables) {
      c(pre_var_index, variables) %<-% find_variable(pre_var$name, variables);

      for (i in 1:length(mutator_functions)) {
        mutator <- mutator_functions[[i]];

        mutator$arguments <- replace(mutator$arguments, mutator$arguments==pre_var$id, variables[[pre_var_index]]$id)

        if (variables[[pre_var_index]]$name == variables[[var_index]]$name) {
          mutator$breakpoint <- new_var_id;
        }

        mutator_functions[[i]] <- mutator;
      }

      if (variables[[pre_var_index]]$id != new_var_id && !(variables[[pre_var_index]]$id %in% variables[[var_index]]$precursors)) {
        variables[[var_index]]$precursors <- append(variables[[var_index]]$precursors, variables[[pre_var_index]]$id);
      }
    }

    mutator <- mutator_functions[[1]];

    functions[[length(functions) + 1]] <- mutator;

    for (hyp in invoked_hypotheses) {
      c(hyp_index, hypotheses) %<-% find_hypothesis(parse(text=hyp$name)[[1]], hypotheses, add = TRUE);

      hypotheses[[hyp_index]]$functions <- append(hypotheses[[hyp_index]]$functions, hyp$functions);

      hypotheses[[hyp_index]]$models <- append(hypotheses[[hyp_index]]$models, new_var_id);
    }

  # # Is "[" call
  # } else if (is.call(exp) && identical(exp[[1]], quote(`[`))) {
  #   c(variables, functions, hypotheses) %<-% recursion(exp[[2]], variables, functions, hypotheses,
  #                                                      assignment_mode = assignment_mode, depth = depth);
  #
  # # Is "$" call
  # } else if (is.call(exp) && identical(exp[[1]], quote(`$`))) {
  #   # Skip retrievals of property, but continue upon the variable
  #   if (lookup_mode) {
  #     # TODO: search for the hypothesis compliance
  #
  #   } else {
  #     c(variables, functions, hypotheses) %<-% recursion(exp[[2]], variables, functions, hypotheses,
  #                                                        assignment_mode = assignment_mode, depth = depth);
  #   }

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
    func <- list();

    func$id <- UUIDgenerate();
    func$name <-as.character(exp[[1]]);
    func$arguments <- list();

    if (length(exp) > 1) {
      if (identical(exp[[1]], quote(`$`)) || identical(exp[[1]], quote(`[`))) {
        end_at = 2;
      } else {
        end_at = length(exp)
      }

      for (arg in as.list(exp)[2:end_at]) {
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

          func$arguments <- append(func$arguments, hypotheses[[hyp_index]]$id);
          hypotheses[[hyp_index]]$functions = append(hypotheses[[hyp_index]]$functions, func$id);

        } else {
          before <- length(functions);

          c(variables, functions, hypotheses) %<-% recursion(arg, variables, functions, hypotheses,
                                                             assignment_mode = assignment_mode, depth = depth);

          if (length(functions) != before) {
            func$arguments <- append(func$arguments, functions[[length(functions)]]$arguments);

            functions[[length(functions)]] <- NULL;
          }
        }
      }
    }

    functions[[length(functions) + 1]] <- func;

  # Is variable name call
  } else if (is.name(exp)) {
    # print(as.character(exp));

    # NB: does not add the variable to the list
    c(tmp, variables) %<-% find_variable(as.character(exp), variables, add = assignment_mode);

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

