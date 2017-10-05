library(mlbench)
data(BostonHousing, package = "mlbench", envir = environment())

test.tasks = DictionaryTasks$new()

test.tasks$add(
  TaskClassif$new(id = "clm.num", data = iris[seq(1, 150, 3),], target = "Species"),
)

test.tasks$add(
  TaskRegr$new(id = "regr.num", data = BostonHousing[1:30, 11:14], target = "medv")
)


