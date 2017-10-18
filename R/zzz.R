#' @import callr
#' @import checkmate
#' @import data.table
#' @import stringi
#' @import evaluate
#' @import phng
#' @importFrom stats setNames predict
#' @importFrom utils data
#' @importFrom BBmisc vlapply viapply vcapply vnapply seq_row seq_col isFALSE
#' @importFrom parallelMap parallelMap parallelExport parallelLibrary
#' @importFrom glue glue collapse single_quote
#' @importFrom R6 R6Class
#' @importFrom digest digest
#' @keywords internal
"_PACKAGE"

.onLoad = function(libname, pkgname) { #nocov start
  backports::import(pkgname)
  parallelMap::parallelRegisterLevels(package = "mlrng", levels = c("resample", "benchmark", "tune"))

  opts = options()
  opts.mlrng = list(
    mlrng.verbose = TRUE,
    mlrng.debug = FALSE,
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
