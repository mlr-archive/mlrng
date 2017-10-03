#' @include Dictionaries.R

mlr.learners$add(
  LearnerClassif$new(
    type = "classif",
    name = "cforest",
    package = "party",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ntree", lower = 1L, default = 500L),
      makeIntegerLearnerParam(id = "mtry", lower = 1L, default = 5L),
      makeLogicalLearnerParam(id = "replace", default = FALSE),
      makeNumericLearnerParam(id = "fraction", lower = 0, upper = 1, default = 0.632,
        requires = quote(replace == FALSE)),
      makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE),
      makeDiscreteLearnerParam(id = "teststat", values = c("quad", "max"), default = "quad"),
      makeDiscreteLearnerParam(id = "testtype",
        values = c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic"),
        default = "Univariate"),
      makeNumericLearnerParam(id = "mincriterion", lower = 0, default = 0),
      makeIntegerLearnerParam(id = "minsplit", lower = 1L, default = 20L),
      makeIntegerLearnerParam(id = "minbucket", lower = 1L, default = 7L),
      makeLogicalLearnerParam(id = "stump", default = FALSE),
      makeIntegerLearnerParam(id = "nresample", lower = 1L, default = 9999L),
      makeIntegerLearnerParam(id = "maxsurrogate", lower = 0L, default = 0L),
      makeIntegerLearnerParam(id = "maxdepth", lower = 0L, default = 0L),
      makeLogicalLearnerParam(id = "savesplitstats", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(),
    properties = c("twoclass", "multiclass", "prob", "factors", "numerics", "ordered", "weights", "missings", "featimp"),
    train = function(task, subset, ...) {
      f = task$formula
      d = getTaskData(task)
      defaults = getDefaults(self$par.set)
      if (is.null(self$par.vals$teststat)) self$par.vals$teststat = defaults$teststat
      if (is.null(self$par.vals$testtype)) self$par.vals$testtype = defaults$testtype
      if (is.null(self$par.vals$mincriterion)) self$par.vals$mincriterion = defaults$mincriterion
      if (is.null(self$par.vals$replace)) self$par.vals$replace = defaults$replace
      if (is.null(self$par.vals$fraction)) self$par.vals$fraction = defaults$fraction
      ctrl = learnerArgsToControl(party::cforest_control,
        ntree = self$par.vals$ntree, mtry = self$par.vals$mtry,
        replace = self$par.vals$replace, fraction = self$par.vals$fraction,
        trace = self$par.vals$trace, teststat = self$par.vals$teststat,
        testtype = self$par.vals$testtype, mincriterion = self$par.vals$mincriterion,
        minsplit = self$par.vals$minsplit, minbucket = self$par.vals$minbucket,
        stump = self$par.vals$stump, nresample = self$par.vals$nresample,
        maxsurrogate = self$par.vals$maxsurrogate, maxdepth = self$par.vals$maxdepth,
        savesplitstats = self$par.vals$savesplitstats)
      party::cforest(f, data = d, controls = ctrl, ...) #FIXME add weights
    },
    predict = function(model, task, subset, ...) {
      data = task$data(subset, setdiff(task$active.cols, task$target))
      pt = self$predict.type
      if (pt == "prob") {
        p = predict(model, newdata = data, type = "prob", ...)
        # FIXME: this will break for nrow(.newdata) == 1? do not use sapply!
        p = t(sapply(p, "["))
        colnames(p) = .model$task.desc$class.levels
      } else {
        p = predict(model, newdata = data, ...)
      }
      p
    }
  )
)
