# dummmy learner that can produce warnings/errors/messages
lrn.mock.regr = LearnerRegr$new(
  name = "mock",
  par.set = ParamHelpers::makeParamSet(
    ParamHelpers::makeDiscreteParam("method", values = c("mean", "median"), default = "mean"),
    ParamHelpers::makeLogicalParam("message", default = FALSE),
    ParamHelpers::makeLogicalParam("warning", default = FALSE),
    ParamHelpers::makeLogicalParam("error", default = FALSE)
  ),
  par.vals = list(),
  properties = c("missings", "factors", "numerics"),
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

