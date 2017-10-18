test.learner = DictionaryLearners$new()

local({
  # dummmy learner that can produce warnings/errors/messages
  lrn = LearnerRegr$new(
    name = "mock",
    par.set = ParamSetFlat$new(params = list(
      ParamCategorical$new("method", values = c("mean", "median"), default = "mean"),
      ParamFlag$new("message", default = FALSE),
      ParamFlag$new("warning", default = FALSE),
      ParamFlag$new("error", default = FALSE)
    )),
    par.vals = list(),
    properties = c("missings", "feat.factor", "feat.numeric"),

    train = function(task, subset, message = FALSE, warning = FALSE, error = FALSE, ...) {
      if (message)
        message("dummy message")
      if (warning)
        warning("dummy warning")
      if (error)
        stop("dummy error")
      return(0)
    },

    predict = function(model, newdata, ...) {
      return(rep(0, nrow(newdata)))
    }
  )
  test.learner$add(lrn)
})
