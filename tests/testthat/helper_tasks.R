test.tasks = DictionaryTasks$new()

local({
  data = iris[seq(1, 150, 3), ]
  data$rowid = sprintf("row%03i", 1:nrow(data))
  b = BackendLocal$new(data, "rowid")
  test.tasks$add(
    TaskClassif$new(id = "clm.num", data = b, target = "Species")
  )
})

local({
  ee = new.env()
  data(BostonHousing, package = "mlbench", envir = ee)
  test.tasks$add(
    TaskRegr$new(id = "regr.num", data = ee$BostonHousing[1:30, 11:14], target = "medv")
  )
})
