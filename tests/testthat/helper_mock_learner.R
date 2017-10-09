# dummmy learner that can produce warnings/errors/messages
lrn.mock.regr = LearnerRegr$new(
  name = "mock",
  par.set = ParamSetFlat$new(params = list(
    ParamCategorical$new("method", values = c("mean", "median"), default = "mean"),
    ParamFlag$new("message", default = FALSE),
    ParamFlag$new("warning", default = FALSE),
    ParamFlag$new("error", default = FALSE)
  )),
  par.vals = list(),
  properties = c("missings", "feat.factor", "feat.numeric"),
  train = function(task, subset, method = "mean", message = FALSE, warning = FALSE, error = FALSE, ...) {
    tn = unlist(task$data(subset, task$target))
    mod = switch(method,
      "mean" = mean(tn),
      "median" = median(tn),
      stop("Illegal value for 'method'"))
    class(mod) = c("dummy.model", class(mod))
    if (message)
      message("dummy message")
    if (warning)
      warning("dummy warning")
    if (error)
      stop("dummy error")

    mod
  },

  predict = function(model, newdata, ...) {
    as.numeric(model$rmodel)
  }
)

