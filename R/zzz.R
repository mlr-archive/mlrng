#' @import checkmate
#' @import data.table
#' @import stringi
#' @import ParamHelpers
#' @import evaluate
#' @importFrom stats setNames
#' @importFrom utils data
#' @importFrom BBmisc vlapply viapply vcapply vnapply seq_row seq_col
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
    mlrng.debug   = TRUE,
    mlrng.keep.train.output = FALSE
  )
  unset = !(names(opts.mlrng) %in% names(opts))
  if (any(unset)) options(opts.mlrng[unset])
} #nocov end


.onUnload = function(libpath) { #nocov start
  pm.opts = getOption("parallelMap.registered.levels")
  pm.opts$mlrng = NULL
  options(parallelMap.registered.levels = pm.opts)
} #nocov end
