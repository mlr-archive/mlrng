#' @title ResampleResult
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} containing data of a \code{\link{resample}}.
#'
#' @field aggr [\code{named numeric}]: Aggregated performance measures.
#' @field data [\code{data.table}]: Data stored in a tabular format.
ResampleResult = R6Class("ResampleResult",
  cloneable = FALSE,
  public = list(
    data = NULL,
    initialize = function(experiments) {
      self$data = rbindlist(experiments)
    }
  ),
  active = list(
    aggr = function() {
      vnapply(rbindlist(self$data$performance), mean)
    }
  )
)
