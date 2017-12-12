context("mlrng config file")

test_that("reading/writing works", {
  fn = tempfile(pattern = "conf", fileext = "yml")
  withr::with_options(list(mlrng.myspecialoption = 42), {
    writeMlrngConfig(fn)
  })
  conf = readMlrngConfig(fn)
  expect_list(conf, names = "unique")
  expect_true(all(stri_startswith_fixed(names(conf), "mlrng")))
  expect_equal(conf$mlrng.myspecialoption, 42)
  expect_true(all(stri_detect_fixed(readLines(fn), ":")))
  writeMlrngConfig(fn)
  conf = readMlrngConfig(fn)
  expect_false("mlrng.myspecialoption" %in% names(conf))
})
