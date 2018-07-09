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

  for (row in exp) {
    if (!is.language(row))
      next;

    c(variables, functions) %<-% recursion(row, variables, functions)
  }

  return(list(variables, functions));
}


find_var <- function (name, variables) {
  var <- NULL;
  index <- length(variables) + 1;

  if (length(variables) > 0)
    for (i in 1:length(variables)) {
      old_var <- variables[[i]];

      if (old_var$name == name) {
        var <- old_var;
        index <- i;

        break;
      }
    }

  if (index > length(variables)) {
    var <- vector(mode="list");

    var$id <- UUIDgenerate();
    var$name <- name;
    var$functions <- list();
    var$precursors <- list();

    variables[[index]] <- var;
  }

  return(list(index, variables))
}


recursion <- function (exp, variables, functions) {
  # Is assignment row
  if (is.call(exp) && (identical(exp[[1]], quote(`<-`)) || identical(exp[[1]], quote(`=`)))) {
    # Skip assignments to the property
    if (is.call(exp[[2]])) {
      return(list(variables, functions))
    }

    # Get or create the variable
    c(var_index, variables) %<-% find_var(as.character(exp[[2]]), variables)

    # Find the precursors and invoked functions
    c(precursor_variables, mutator_functions) %<-% recursion(exp[[3]], list(), list());

    # Add the newly gathered data to the larger massif
    for (pre_var in precursor_variables) {
      c(pre_var_index, variables) %<-% find_var(pre_var$name, variables)

      if (variables[[pre_var_index]]$name == variables[[var_index]]$name) {
        # TODO: decide what to do with self-references
        next;
      }

      if (!(variables[[pre_var_index]]$id %in% variables[[var_index]]$precursors))
        variables[[var_index]]$precursors <- append(variables[[var_index]]$precursors, variables[[pre_var_index]]$id);

      variables[[pre_var_index]]$functions <- append(variables[[pre_var_index]]$functions, mutator_functions);
    }

  # Is "$" call
  } else if (is.call(exp) && identical(exp[[1]], quote(`$`))) {
    # Skip retrievals of property
    c(variables, functions) %<-% recursion(exp[[2]], variables, functions);

  # Is function declaration
  } else if (is.call(exp) && identical(exp[[1]], quote(`function`))) {
    # TODO: introduce scopes

    # functions[[length(functions) + 1]] <- exp;

  # Is if/for clause
  } else if (is.call(exp) && (identical(exp[[1]], quote(`if`)) || identical(exp[[1]], quote(`for`)))) {
    # TODO: add conditional as the precursor?

    c(variables, functions) %<-% recursion(exp[[3]], variables, functions);

  # Is block statement
  } else if (is.call(exp) && identical(exp[[1]], quote(`{`))) {
    for (line in as.list(exp)[2:length(exp)])
      c(variables, functions) %<-% recursion(line, variables, functions);

  # Is a generic function call
  } else if (is.call(exp)) {
    func <- vector(mode="list");

    func$id <- UUIDgenerate();
    func$name <-as.character(exp[[1]]);
    func$arguments <- list();

    if (length(exp) > 1) {
      for (arg in as.list(exp)[2:length(exp)]) {
        if (missing(arg)) {
          # next;  # is redunddant

        } else if (is.atomic(arg)) {
          func$arguments <- append(func$arguments, arg)

        } else if (is.name(arg)) {
          c(var_index, variables) %<-% find_var(as.character(arg), variables);

          func$arguments <- append(func$arguments, variables[[var_index]]$id)

        } else {
          c(variables, functions) %<-% recursion(arg, variables, functions);

          func$arguments <- append(func$arguments, 'CALL');
        }
      }
    }

    functions[[length(functions) + 1]] <- func

  # Is variable name call
  } else if (is.name(exp)) {
    print(as.character(exp));
    c(tmp, variables) %<-% find_var(as.character(exp), variables);

  # Is atomic
  } else if (is.atomic(exp)) {

  # Dunno
  } else {

  }

  return(list(variables, functions))
}


addin <- function() {
  ui <- miniPage(
    gadgetTitleBar("Data structure"),
    miniContentPanel(
      uiOutput("graph")
    )
  )

  server <- function(input, output, session) {
    invalidatePeriodically <- reactiveTimer(intervalMs = 1000);
    old_path <- "";

    observe({
      invalidatePeriodically()

      c(id, path, textContents, selections) %<-% rstudioapi::getActiveDocumentContext()[0:4];

      if (path != old_path) {
        #selections <- lapply(selections, unwrapSelections);
        stuff <- loop(textContents);

        output$graph <- renderUI({
          pre(HTML(jsonlite::toJSON(stuff, auto_unbox = TRUE, pretty = TRUE)))
        })

        old_path <<- path;
      }
    })
  }

  viewer <- paneViewer(300);

  runGadget(ui, server, viewer = viewer);
}

