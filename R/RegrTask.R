#' @export
RegrTask = R6Class("ClassifTask",
  inherit = SupervisedTask,
  public = list(
    type = "regr",
    initialize = function(data, target = NA_character_, cols = NULL, id.col = NULL, id = deparse(substitute(data))) {
      super$initialize(data, target = target, cols = cols, id.col = id.col, id = id)
      assertNumeric(self$backend[[target]], finite = TRUE, any.missing = FALSE)
    }
  )
)
