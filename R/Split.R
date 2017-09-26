#' @title Split
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} containing data of a split into training and test set.
#'
#' @field train [\code{integer}]: Training indices
#' @field test [\code{integer}]: Test indices
Split = R6Class("Split",
  public = list(
    train.bit = NULL,
    test.bit = NULL,
    initialize = function(train, test = NULL) {
      if (!is.bit(train))
        assertLogical(train, any.missing = FALSE)
      if (!is.null(test) && !is.bit(test))
        assertLogical(test, any.missing = FALSE)

      self$train.bit = as.bit(train)
      self$test.bit = as.bit(test %??% !self$train.bit)
    }
  ),

  active = list(
    train = function() as.integer(as.which(self$train.bit)),
    test = function() as.integer(as.which(self$test.bit))
  )
)
