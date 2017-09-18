Split = R6Class("Split",
  private = list(
    train.bit = NULL,
    test.bit = NULL
  ),
  public = list(
    initialize = function(train, test = NULL) {
      private$train.bit = as.bit(train)
      private$test.bit = as.bit(test %??% !private$train.bit)
    }
  ),
  active = list(
    train = function() as.which(private$train.bit),
    test = function() as.which(private$test.bit)
  )
)
