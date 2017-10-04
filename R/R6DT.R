
R6DT = R6Class("R6DT",
  public = list(
    dt = NULL,

    initialize = function(..., .dt = NULL, .slots) {
      if (is.null(.dt))
        self$dt = do.call(data.table, lapply(list(...), list))
      else
        self$dt = .dt
    },

    dtgrow = function(..., .dt = NULL) {
      if (is.null(.dt))
        .dt = do.call(data.table, lapply(list(...), list))
      self$dt = cbind(self$dt, .dt)
    }
  )
)

