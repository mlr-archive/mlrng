#' @import checkmate
#' @import data.table
#' @import stringi
#' @import ParamHelpers
#' @importFrom glue glue
#' @importFrom R6 R6Class
#' @importFrom parallelMap parallelMap parallelExport parallelLibrary
#' @importFrom BBmisc vlapply vcapply seq_row seq_col
#' @keywords internal
"_PACKAGE"

.onLoad = function(libname, pkgname) { #nocov start
  backports::import(pkgname)
  parallelMap::parallelRegisterLevels(package = "mlrng",
    levels = c("resample", "benchmark", "tune"))
} #nocov end


.onUnload = function(libpath) { #nocov start
  pm.opts = getOption("parallelMap.registered.levels")
  pm.opts$mlrng = NULL
  options(parallelMap.registered.levels = pm.opts)
} #nocov end


mlrng = new.env(parent = emptyenv())
mlrng$learner.properties = c("missings", "factors", "numerics")
mlrng$learner.types = c("classif", "regr")
mlrng$debug = FALSE
