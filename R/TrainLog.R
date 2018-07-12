#' @title TrainLog
#' @format [R6Class()] object
#'
#' @description
#' A [R6::R6Class()] containing logging information of a [train()] call.
#' Constructor is called with output [`list`]: List of output, messages, warnings and errors occured in training and the training time.
#' @field train.time [`numeric(1)`]: Training time.
#' @field messages [`list`]: List of messages encountered while training the model.
#' @field warnings [`list`]: List of warnings encountered while training the model.
#' @field errors [`list`]: List of errors encountered while training the model.
#' @field output [`list`]: Remaining output from model fit, only saved if `mlrng.keep.train.output` option is set to `TRUE`.
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
    },
    print = function(...) {
     gcat("Training log with {self$n.errors} errors, {self$n.warnings} warnings and {self$n.messages} messages.")
     if (getOption("mlrng.debug", FALSE))
      cat("\n", format(self),  "\n")
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
