test_that("parsing works on examples", {
  expect_parse <- function (fileName) {

    uuidCounter <<- 0
    UUIDgenerate <<- function () {
        uuidCounter <<- uuidCounter + 1
        uuidCounter
    }

    path <- paste0("../snapshots/", fileName, ".R")
    code <- readChar(path, file.info(path)$size)

    eval_ <<- FALSE
    c(variables, functions, hypotheses) %<-% parse(text=code, line_non=1, interactive=FALSE, variables=NULL, functions=NULL, hypotheses=NULL)

    c(expected_variables, expected_functions, expected_hypotheses) %<-% readRDS(paste0(path, "-snapshot"))
    
    expected_variables$id <- variables$id
    variables$precursor <- NULL
    expected_variables$precursor <- NULL
    expect_equal(variables, expected_variables)
    expected_functions$id <- functions$id
    expect_equal(functions, expected_functions)
    expected_hypotheses$id <- hypotheses$id
    expect_equal(hypotheses, expected_hypotheses)
  }

  expect_parse(
    "intermediate-kbd-study"
  );
  expect_parse(
    "4DoingTestsOfAssumptions"
  );
  expect_parse(
    "7DoingFactorialANOVAs"
  )
})
