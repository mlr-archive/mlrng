#' @include Dictionary.R

#' @export
Measures = Dictionary$new("Measure")

#' @export
listMeasures = function() {
  tab = rbindlist(eapply(Measures$storage, function(x) {
    list(
      id = x$id,
      requires = list(x$requires)
    )
  }, USE.NAMES = FALSE))
  setkeyv(tab, "id")[]
}
