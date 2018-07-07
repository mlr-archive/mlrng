#' @title Base Class for TuneControl
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to represent TuneControl
#'
#' @section Member Variables:
#'
#' \describe{
#'   \item{budget}{[\code{integer}] \cr
#'     Maximum number of evaluations of different hyperparameter settings carried out through resampling}
#'   \item{par.set}{[\code{\link[phng]{ParamSet}}] \cr
#'     The paramer space that should be searched through.}
#'   \item{resampling}{[\code{Resampling}] \cr
#'     The resampling object.}
#'   \item{opt.path}{[\code{\link[phng]{OptPath}}] \cr
#'     Holds the Optimization Path that is generated during tuning.}
#' }
#'
#' @return [\code{\link{TuneControl}}].
#' @family TuneControl
#' @export
#'
TuneControl = R6Class("TuneControl",
  cloneable = FALSE,
  public = list(
    budget = NA_integer_,
    par.set = NULL,
    resampling = NULL,
    opt.path = NULL,
    initialize = function(par.set, budget, resampling) {
      self$par.set = assertClass(par.set, "ParamSet")
      self$budget = assertCount(budget)
      self$resampling = assertClass(getResampling(resampling), "Resampling")
    }
  )
)
