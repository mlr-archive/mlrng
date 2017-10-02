#' @title TrainLog
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} containing logging information of a \code{\link{train}} call.
#'
#' @field raw.log [\code{list}]: List of output, messages, warnings and errors occured in training.
TrainLog = R6Class("TrainLog",
  public = list(
    train.time = NULL,
    messages = NULL,
    warnings = NULL,
    errors = NULL,
    output = NULL,
    initialize = function(raw.log) {

        assertList(raw.log)

        if(getOption("mlrng.keep.train.output", FALSE))
          self$output =raw.log[vlapply(raw.log, is.character)]
        self$errors = raw.log[vlapply(raw.log, evaluate::is.error)]
        self$warnings = raw.log[vlapply(raw.log, evaluate::is.warning)]
        self$messages = raw.log[vlapply(raw.log, evaluate::is.message)]
        self$train.time = attr(raw.log[[1]]$src, "timing")[3]
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
