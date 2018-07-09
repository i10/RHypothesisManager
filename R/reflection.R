library(miniUI);
#library(rstudioapi);
library(shiny);

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


generate_var_id <- function () {
  #TODO: introduce UUIDs
  return(as.integer(Sys.time()))
}


recursion <- function (exp, variables, functions) {
  # Is assignment row
  if (is.call(exp) && (identical(exp[[1]], quote(`<-`)) || identical(exp[[1]], quote(`=`)))) {
    # Skip assignments to the property
    if (is.call(exp[[2]])) {
      return(list(variables, functions))
    }

    # Get or create the variable
    new_var <- NULL;
    was_new_var <- FALSE;
    
    for (old_var in variables) {
      if (old_var$name == as.character(exp[[2]])) {
        new_var <- old_var;
        
        break;
      }
    }
    
    if (is.null(new_var)) {
      was_new_var <- TRUE;
      new_var <- vector(mode="list");
      
      new_var$id <- generate_var_id();
      new_var$name <- as.character(exp[[2]]);
      new_var$functions <- list();
    }
    
    # Find the precursors and invoked functions
    c(precursor_variables, f) %<-% recursion(exp[[3]], list(), list());
    
    # Add the newly gathered data to the larger massif
    for (var in precursor_variables) {
      if (var$name == new_var$name) {
        # TODO: decide on the course of action
        next;
      }
      
      for (old_var in variables) {
        if (var$name == old_var$name) {
          var <- old_var
          break;
        }
      }
      
      #new_var$precursors[[length(new_var$precursors) + 1]] <- var$id;
      new_var$precursors[[length(new_var$precursors) + 1]] <- var$name;
      var$functions <- append(var$functions, f);
    }
    
    if (was_new_var) {
      variables[[length(variables) + 1]] <- new_var;
    }
    
    # Is function declaration
  } else if (is.call(exp) && identical(exp[[1]], quote(`function`))) {
    # Skip?
    
    # functions[[length(functions) + 1]] <- exp;
    
    # Is function call
  } else if (is.call(exp)) {
    functions <- append(functions, as.character(exp[[1]]))
    
    if (length(exp) > 1) {
      for (arg in as.list(exp)[2:length(exp)]) {
        if (missing(arg)) {
          next;
        }
        
        c(variables, functions) %<-% recursion(arg, variables, functions);
      }
    }
    
    # Is variable name call
  } else if (is.name(exp)) {
    is_existing_var <- FALSE;
    
    for (v in variables) {
      if (v$name == as.character(exp)) {
        is_existing_var <- TRUE;
        break;
      }
    }
    
    if (!is_existing_var) {
      new_var <- vector(mode="list")
      
      new_var$id <- generate_var_id();
      new_var$name <- as.character(exp);
      new_var$functions <- list();
      
      variables[[length(variables) + 1]] <- new_var;
    }
    
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
    invalidatePeriodically <- reactiveTimer(intervalMs = 1000)
    
    observe({
      invalidatePeriodically()
      
      c(id, path, textContents, selections) %<-% rstudioapi::getActiveDocumentContext()[0:4];
      
      #selections <- lapply(selections, unwrapSelections);
      
      stuff <- loop(textContents);
      
      output$graph <- renderUI({
        pre(HTML(jsonlite::toJSON(stuff, auto_unbox = TRUE, pretty = TRUE)))
      })
    })
  }
  
  viewer <- paneViewer(300);
  
  runGadget(ui, server, viewer = viewer);
}

