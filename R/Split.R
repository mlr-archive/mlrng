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
      assertAtomicVector(train.set, min.len = 1L, any.missing = FALSE)
      self$train.set = train.set
      self$test.set = test.set %??% vector(typeof(train.set), 0L)
    }
  )
)
