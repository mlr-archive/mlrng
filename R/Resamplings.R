#' @include Dictionary.R

#' @export
Resamplings = Dictionary$new("Resampling")

#' @export
listResamplings = function() {
  tab = rbindlist(eapply(Resamplings$storage, function(r) {
    list(
      id = r$id,
      description = r$description,
      pars = list(r$pars)
    )
  }, USE.NAMES = FALSE))
  setkeyv(tab, "id")[]
}
