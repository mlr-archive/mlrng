#' @title TrainLog
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} containing logging information of a \code{\link{train}} call.
#' Constructor is called with output [\code{list}]: List of output, messages, warnings and errors occured in training and the training time.
#' @field train.time [\code{numeric(1)}]: Training time.
#' @field messages [\code{list}]: List of messages encountered while training the model.
#' @field warnings [\code{list}]: List of warnings encountered while training the model.
#' @field errors [\code{list}]: List of errors encountered while training the model.
#' @field output [\code{list}]: Remaining output from model fit, only saved if \code{mlrng.keep.train.output} option is set to \code{TRUE}.
TrainLog = R6Class("TrainLog",
  public = list(
    train.time = NULL,
    messages = NULL,
    warnings = NULL,
    errors = NULL,
    output = NULL,
    initialize = function(output, train.time) {

        assertList(output)

        if(getOption("mlrng.keep.train.output", FALSE))
          self$output = assertList(output)
        self$errors = output[vlapply(output, evaluate::is.error)]
        self$warnings = output[vlapply(output, evaluate::is.warning)]
        self$messages = output[vlapply(output, evaluate::is.message)]
        self$train.time = assertNumeric(train.time)
    }),
  active = list(
    n.errors = function() {
      length(self$errors)
    },
    n.warnings = function() {
      length(self$warnings)
    },
    n.messages = function() {
      length(self$messages)
    }
  )
)
