
R6DT = R6Class("R6DT",
  public = list(
    dt = NULL,

    initialize = function(...) {
      self$dt = do.call(data.table, lapply(list(...), list))
    },

    dtgrow = function(...) {
      dt = do.call(data.table, lapply(list(...), list))
      self$dt[, names(dt) := dt]
      invisible(self)
    }
  )
)

