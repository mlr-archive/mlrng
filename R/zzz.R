#' @import callr
#' @import checkmate
#' @import data.table
#' @import evaluate
#' @importFrom BBmisc vlapply viapply vcapply vnapply seq_row seq_col isFALSE
#' @importFrom digest digest
#' @importFrom fastmatch fmatch %fin%
#' @importFrom glue glue double_quote
#' @importFrom parallelMap parallelMap parallelExport parallelLibrary
#' @importFrom prettyunits pretty_sec
#' @importFrom R6 R6Class
#' @importFrom stats setNames predict
#' @importFrom utils data
#' @import phng
#' @import stringi
#' @keywords internal
"_PACKAGE"

.onLoad = function(libname, pkgname) { #nocov start
  backports::import(pkgname)
  backports::import(pkgname, "hasName", force = TRUE)
  parallelMap::parallelRegisterLevels(package = "mlrng", levels = c("resample", "benchmark", "tune"))

  opts = options()
  opts.mlrng = list(
    mlrng.verbose = TRUE,
    mlrng.debug = TRUE,
    mlrng.keep.train.output = FALSE,
    mlrng.continue.on.learner.error = FALSE,
    mlrng.train.encapsulation = 1
  )
  unset = !(names(opts.mlrng) %in% names(opts))
  if (any(unset)) options(opts.mlrng[unset])
} #nocov end


.onUnload = function(libpath) { #nocov start
  pm.opts = getOption("parallelMap.registered.levels")
  pm.opts$mlrng = NULL
  options(parallelMap.registered.levels = pm.opts)
} #nocov end

mlrng = new.env(parent = emptyenv())
mlrng$supported.col.types = c("logical", "integer", "numeric", "character", "factor", "ordered")
mlrng$supported.learner.props = c(
  sprintf("feat.%s", mlrng$supported.col.types),
  "missings", "weights", "twoclass", "multiclass", "prob", "featimp", "parallel",
  "formula", "oobpreds", "se"
)
mlrng$result.states = c("Result", "TrainResult", "PredictResult", "PerformanceResult", "ResampleResult", "BenchmarkResult")
