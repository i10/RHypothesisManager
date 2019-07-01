test_that("parsing works on examples", {
  expect_parse <- function (code, expected_variables, expected_functions, expected_hypotheses) {
    eval_ <<- FALSE;
    c(variables, functions, hypotheses) %<-% parse(text=code, line_non=1, interactive=FALSE, variables=NULL, functions=NULL, hypotheses=NULL);
    
    variables$id <- NULL;
    expected_variables$id <- NULL;
    expect_equal(variables, expected_variables);
    functions$id <- NULL;
    expected_functions$id <- NULL;
    expect_equal(functions, expected_functions);
    hypotheses$id <- NULL;
    expected_hypotheses$id <- NULL;
    expect_equal(hypotheses, expected_hypotheses);
  }

  # Helpers
  make_variables <- function (variables) {
    variables <- data.frame(matrix(variables, ncol = 8, nrow = 0));
    colnames(variables) <- c("id", "name", "precursors", "columns", "origin", "type", "value", "generation");
    return(variables);
  }

  v <- function(id, name="a", precursors=NULL, columns=NULL, origin=NA, type="constant", value=NA, generation=0) {
    if (missing(id)) {
      id <- UUIDgenerate();
    }
    return(c(id, name, precursors, columns, origin, type, value, generation))
  }

  make_functions <- function () {
    functions <- data.frame(matrix(ncol = 8, nrow = 0));
    colnames(functions) <- c("id", "name", "lines", "signature", "packages", "arguments", "depth", "breakpoint");
    return(functions);
  }

  make_hypotheses <- function () {
    hypotheses <- data.frame(matrix(ncol = 6, nrow = 0));
    colnames(hypotheses) <- c("id", "name", "columns", "functions", "models", "formulas");
    return(hypotheses);
  }

  # Test cases
  expect_parse(
    "",
    make_variables(),
    make_functions(),
    make_hypotheses()
  );
  expect_parse(
    "a <- 1",
    make_variables(c(v(name="a"))),
    make_functions(),
    make_hypotheses()
  );
})
