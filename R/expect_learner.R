#' @title Check Learner
#'
#' @description
#' Check if Learner object is valid.
#'
#' @param lrn [[Learner()]]\cr
#'   Object of type [Learner()].
#' @export
expect_learner = function(lrn) {
  testthat::expect_is(lrn, "Learner")
  expect_string(lrn$id, min.chars = 1L)
  expect_character(lrn$packages, min.chars = 1L)
  expect_subset(lrn$properties, mlrng$supported.learner.props)
  testthat::expect_is(lrn$par.set, "ParamSet")
  expect_list(lrn$par.vals, names = "unique")
  expect_function(lrn$predict, args = c("model", "newdata"), ordered = TRUE)
  expect_function(lrn$train, args = c("task", "subset"), ordered = TRUE)
}
