test.learners = DictionaryLearners$new()

local({
  # dummmy learner that can produce warnings/errors/messages
  lrn = LearnerRegr$new(
    name = "mock.conditions",
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
  test.learners$add(lrn)

  # dummy learner that stores training ids
  # lrn = LearnerClassif$new(
  #   name = "mock.rowids",
  #   par.vals = list(),
  #   properties = c("missings", "feat.factor", "feat.numeric"),

  #   train = function(task, subset, ...) {
  #     list(
  #       rowids = task$backend$get(include.rowid.col = TRUE)[[task$backend$rowid.col]],
  #       label = task$classes[1L]
  #     )

  #   },
  #   predict = function(model, newdata, ...) {
  #     rep.int(model$label, nrow(newdata))
  #   }
  # )

  # test.learners$add(lrn)
})
