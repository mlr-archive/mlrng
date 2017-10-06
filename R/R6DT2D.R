
R6DT2D = R6Class("R6DT2D",
  public = list(
    dt = NULL,

    initialize = function(l) {
      self$dt = rbindlist(lapply(l, function(x) x$dt))
    },

    rbind = function(x) {
      self$dt = rbind(self$dt, x$dt)
    }

  ),
  active = list(
    nrow = function() nrow(self$dt),
    ncol = function() ncol(self$dt),
    colnames = function() colnames(dt)
  )
)

