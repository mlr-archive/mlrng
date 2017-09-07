#' @export
ClassifTask = R6Class("ClassifTask",
  inherit = SupervisedTask,
  public = list(
    type = "classif",
    positive = NA_character_,
    initialize = function(data, target, cols = NULL, id.col = NULL, positive = NULL, id = deparse(substitute(data))) {
      super$initialize(data, target = target, cols = cols, id = id, id.col = id.col)
      target = self$backend[[target]]
      assertFactor(target, any.missing = FALSE)
      if (!is.null(positive)) {
        nlevs = nlevels(target)
        if (nlevs > 2L)
          gstop("Cannot set a positive class for multilabel classification with {nlevs} levels")
        self$positive = assertChoice(positive, levels(target))
      }
    }
  ),

  active = list(
    levels = function() levels(self$backend[[self$target]])
  )
)
