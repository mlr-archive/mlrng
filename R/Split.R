#' @title Split
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} containing data of a split into training and test set.
#'
#' @field train [\code{integer}]: Training row ids
#' @field test [\code{integer}]: Test row ids
Split = R6Class("Split",
  public = list(
    train.set = NULL,
    test.set = NULL,
    initialize = function(train.set, test.set = NULL) {
      self$train.set = as.bit(train.set)
      self$test.set = as.bit(test.set %??% bit(length(train.set)))
    }
  )
)
