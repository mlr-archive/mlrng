#' @export
predict.TrainResult = function(object, task = object$task, subset = NULL, ...) {
  if (...length() > 0L)
    stop("predict: dotargs currently unsupported!")
  assertTask(task)
  row.ids = task$row.ids(subset)

  predictWorker(object, task = task, row.ids)
}

predictWorker = function(train.result, task, row.ids) {
  if (train.result$train.success) {
    requireNS(learner$packages)
    learner = train.result$learner
  } else {
    learner = createFallbackLearner(task)
  }
  pars = c(list(model = train.result, task = task, row.ids = row.ids), learner$par.vals)
  result = do.call(learner$predict, pars)
  PredictResult$new(train.result, row.ids, result)
}

predictNewData = function(train.result, newdata) {
  train.result = train(mlr.tasks$get("iris"), mlr.learners$get("classif.rpart"))
  newdata = as.data.table(iris[1:20, ])
  newdata$Species = NULL

  task = train.result$task
  if (train.result$train.success) {
    learner = train.result$learner
    requireNS(learner$packages)
  } else {
    learner = createFallbackLearner(task)
  }

  if (task$backend$rowid.col %chnin% names(newdata))
    newdata[[task$backend$rowid.col]] = sprintf("newdata.%i", seq_row(newdata))
  newdata[[task$target]] = NA_character_
  newtask = task$clone(deep = TRUE)
  newtask$data = newdata
}

# createMakeShiftTask = function(newdata, task) {
#   newdata = subset(iris[100:130, ], select = -Species)
#   task = mlr.tasks$get("iris")

#   cn = names(newdata)
#   target = task$target
#   features = task$features
#   rowid.col = task$backend$rowid.col

#   assertNames(cn, must.include = features)
#   if (task$backend$rowid.col %nin% cn)
#     newdata[[rowid.col]] = sprintf("newdata.%i", seq_len(nrow(newdata)))
#   if (task$target %nin% cn) # FIXME: won't work for multi-target tasks
#     newdata[[target]] = task$missing.target

# }
