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
    train = NULL,
    test = NULL,
    initialize = function(train, test = NULL) {
      assertAtomicVector(train, any.missing = FALSE)
      self$train = train
      self$test = test %??% vector(typeof(train), 0L)
    }
  )
)
