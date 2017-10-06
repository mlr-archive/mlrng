library(testthat)
library(checkmate)
library(stringi)

private = function(x) {
  if (!R6::is.R6(x))
    stop("Expected R6 class")
  x$.__enclos_env__[["private"]]
}
