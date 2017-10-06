library(R6)

Pipe = R6Class("Pipe",
  public = list(
    data = NULL,
    initialize = function() {
      self$data = data.table(
        id = character(),
        operator = list()
      )
    },

    print = function() {
      n = nrow(self$data)
      gcat("Pipeline with {n} step{plural(n)}")
      gcat("[ ", stri_flatten(self$data$id, " -> "), " ]")
    },

    append = function(op) {
      n = nrow(self$data)
      if (n > 0L) {
        last = self$data$operator[[n]]
        if (last$output != op$input)
          gstop("Operator's input ({op$input}) not compatible to previous output ({last$output})")
      }
      self$data = rbind(self$data, data.table(id = op$id, operator = list(op)))
    },

    train = function(task, subset) {
      for (i in seq_len(nrow(self$data))) {
        op = self$data$operator[[i]]
        if (!is.null(op$train)) {
          res = op$train(task, subset)
          task = res$task
          subset = res$subset
        }
      }
    }

  )
)

Operator = R6Class("Operator",
  public = list(
    id = NULL,
    pars = NULL,
    train = NULL,
    input = NULL,
    output = NULL,
    predict = NULL,
    initialize = function(id, input, output, pars = list(), train = NULL, predict = NULL) {
      self$id      = assertString(id)
      self$input   = assertString(input)
      self$output  = assertString(output)
      self$pars    = assertList(pars, names = "unique")
      if (!is.null(train)) {
        self$train   = assertFunction(train, null.ok = TRUE)
        environment(self$train) = environment(self$initialize)
      }
      if (!is.null(predict)) {
        self$predict = assertFunction(predict, null.ok = TRUE)
        environment(self$predict) = environment(self$initialize)
      }
    }
  )
)

featsel = Operator$new(
  id = "random.featsel",
  input = "trainset",
  output = "trainset",
  pars = list(n = 1),
  train = function(task, subset) {
    new.task = task$clone(deep = TRUE)
    new.task$view$active.cols = c(task$target, sample(task$features, self$pars$n))
    list(task = new.task, subset = subset)
  }
)

subsample = Operator$new(
  id = "subsample",
  input = "trainset",
  output = "trainset",
  pars = list(ratio = 0.5),
  train = function(task, subset) {
    subset = sample(subset, self$pars$ratio * length(subset))
    list(task = task, subset = subset)
  }
)

lrn = Operator$new(
  id = "training",
  input = "trainset",
  output = "model",
  pars = list(lrn = mlr.learners$get("classif.rpart")),
  train = function(task, subset) {
    lrn = self$pars$lrn
    train(task = task, learner = lrn, subset)
  }
)

p = Pipe$new()
p
p$append(subsample)
featsel$pars$n = 3
p$append(featsel)
p$append(lrn)
p


if (FALSE) {
  task = mlr.tasks$get("iris")
  subset = 1:100
  self = list(pars = list(n = 1))
  res = p$train(task, subset)
}
