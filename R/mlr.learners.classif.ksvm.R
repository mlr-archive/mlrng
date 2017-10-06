#' @include Dictionaries.R

mlr.learners$add(
  LearnerClassif$new(
    name = "ksvm",
    package = "kernlab",
    par.set = makeParamSet(
      makeLogicalLearnerParam(id = "scaled", default = TRUE),
      makeDiscreteLearnerParam(id = "type", default = "C-svc", values = c("C-svc", "nu-svc", "C-bsvc", "spoc-svc", "kbb-svc")),
      makeDiscreteLearnerParam(id = "kernel", default = "rbfdot",
        values = c("vanilladot", "polydot", "rbfdot", "tanhdot", "laplacedot", "besseldot", "anovadot", "splinedot")),
      makeNumericLearnerParam(id = "C",
        lower = 0, default = 1, requires = quote(type %in% c("C-svc", "C-bsvc", "spoc-svc", "kbb-svc"))),

      makeNumericLearnerParam(id = "nu",
        lower = 0, default = 0.2, requires = quote(type == "nu-svc")),
      makeNumericLearnerParam(id = "epsilon", default = 0.1,
        requires = quote(type %in% c("eps-svr", "nu-svr", "eps-bsvm"))),
      makeNumericLearnerParam(id = "sigma",
        lower = 0, requires = quote(kernel %in% c("rbfdot", "anovadot", "besseldot", "laplacedot"))),
      makeIntegerLearnerParam(id = "degree", default = 3L, lower = 1L,
        requires = quote(kernel %in% c("polydot", "anovadot", "besseldot"))),
      makeNumericLearnerParam(id = "scale", default = 1, lower = 0,
        requires = quote(kernel %in% c("polydot", "tanhdot"))),
      makeNumericLearnerParam(id = "offset", default = 1,
        requires = quote(kernel %in% c("polydot", "tanhdot"))),
      makeIntegerLearnerParam(id = "order", default = 1L,
        requires = quote(kernel == "besseldot")),
      makeNumericLearnerParam(id = "tol", default = 0.001, lower = 0),
      makeLogicalLearnerParam(id = "shrinking", default = TRUE),
      makeNumericVectorLearnerParam(id = "class.weights", len = NA_integer_, lower = 0),
      makeLogicalLearnerParam(id = "fit", default = TRUE, tunable = FALSE),
      makeIntegerLearnerParam(id = "cache", default = 40L, lower = 1L)
    ),
    par.vals = list(fit = FALSE),
    properties = c("twoclass", "multiclass", "feat.numeric", "feat.factor", "prob"),
    train = function(task, subset, weights = NULL, ...) {
      kpar = learnerArgsToControl(control = list, degree = self$par.vals$degree,
        offset = self$par.vals$offset, scale = self$par.vals$scale, sigma = self$par.vals$sigma,
        order = self$par.vals$order, length = self$par.vals$length, lambda = self$par.vals$lambda,
        normalized = self$par.vals$normalized)
      f = task$formula
      pm = self$predict.type == "prob"
      if (base::length(kpar) > 0L)
        kernlab::ksvm(f, data = getTaskData(task, subset, props = self$properties),
          kpar = kpar, prob.model = pm, ...)
      else
        kernlab::ksvm(f, data = getTaskData(task, subset, props = self$properties),
          prob.model = pm, ...)
    },
    predict = function(model, newdata, ...) { #FIXME: not working right now
      type = switch(self$predict.type, prob = "probabilities", "response")
      kernlab::predict(model$rmodel, newdata = newdata, type = type, ...)
    }
  ))
