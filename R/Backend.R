Backend = R6Class("Backend",
  public = list(
    rowid.col = NULL
  ),

  active = list(
    types = function() {
      vcapply(self$head(1L), class)
    }
  )
)
