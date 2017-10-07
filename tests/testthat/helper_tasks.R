library(mlbench)
data(BostonHousing, package = "mlbench", envir = environment())

test.tasks = DictionaryTasks$new()

local({
  data = iris[seq(1, 150, 3), ]
  data$rowid = sprintf("row%03i", 1:nrow(data))
  v = asView("iris", data, rowid.col = "rowid")
  test.tasks$add(
    TaskClassif$new(id = "clm.num", data = v, target = "Species")
  )
})

test.tasks$add(
  TaskRegr$new(id = "regr.num", data = BostonHousing[1:30, 11:14], target = "medv")
)
