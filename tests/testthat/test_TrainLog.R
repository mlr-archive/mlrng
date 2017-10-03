context("TrainLog")

  logGenerator = function(outputs = 0, warnings = 0, messages = 0, error = FALSE) {
    lapply(seq_len(outputs), function(x) print(gcat("output {x}")))
    lapply(seq_len(warnings), function(x) gwarn("warning {x}"))
    lapply(seq_len(messages), function(x) gmessage("messages {x}"))
    if (error) stop("errors")

    return(TRUE)

  }


test_that("logging without errors", {

  n.out = 1
  n.warn = 2
  n.message = 2
  err = 0

  call.successful = FALSE
  raw.log = evaluate::evaluate("call.successful = logGenerator(n.out, n.warn, n.message, err)", new_device = FALSE)
  log.obj = TrainLog$new(raw.log)
  expect_true(call.successful)
  expect_null(log.obj$output)
  expect_equal(log.obj$n.messages, n.message)
  expect_equal(log.obj$n.warnings, n.warn)
  expect_equal(log.obj$n.errors, err)

})


test_that("logging with error", {

  err = 1

  call.successful = FALSE
  raw.log = evaluate::evaluate("call.successful = logGenerator(error = err)", new_device = FALSE)
  log.obj = TrainLog$new(raw.log)
  expect_false(call.successful)
  expect_null(log.obj$output)
  expect_equal(log.obj$n.messages, 0)
  expect_equal(log.obj$n.warnings, 0)
  expect_equal(log.obj$n.errors, err)

})
