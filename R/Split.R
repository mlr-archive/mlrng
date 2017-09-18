Split = R6Class("Split",
  private = list(
    train.bit = NULL,
    test.bit = NULL
  ),

  public = list(
    initialize = function(train, test = NULL) {
      if (!is.bit(train))
        assertLogical(train, any.missing = FALSE)
      if (!is.null(test) && !is.bit(test))
        assertLogical(test, any.missing = FALSE)
      private$train.bit = as.bit(train)
      private$test.bit = as.bit(test %??% !private$train.bit)
    }
  ),

  active = list(
    train = function() as.which(private$train.bit),
    test = function() as.which(private$test.bit),
    inds = function() length(private$train.bit)
  )
)

#' @export
length.Split = function(x) {
  x$inds
}
