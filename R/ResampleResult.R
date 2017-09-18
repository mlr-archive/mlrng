ResampleResult = R6Class("ResampleResult",
  cloneable = FALSE,
  public = list(
    data = NULL,
    measure = NULL,
    initialize = function(result, measure) {
      self$data = result
      self$measure = measure
    },
    performance = function(id) {
      rbindlist(lapply(rr$data, function(x) as.list(x$performance)))
    }
  ),
  active = list(
    aggr = function() {
      mean(unlist(lapply(self$data, "[[", "performance")))
    }
  )
)

#' @export
as.data.table.ResampleResult = function(x, keep.rownames = FALSE, ...) {
  rbindlist(lapply(x$data, function(x) {
    c(list(
      predicted = list(x$predicted),
      model = list(x$model)
    ), unlist(x$performance))
  }), idcol = "iter")
}
