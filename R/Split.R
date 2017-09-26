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
    test = function() as.integer(as.which(self$test.bit)),
    inds = function() length(self$train.bit)
  )
)

#' @export
length.Split = function(x) {
  x$inds
}
