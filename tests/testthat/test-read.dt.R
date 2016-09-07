context("DataTrac3")

fpath <- system.file("extdata", "DataTrac3.db", package="csdf")
obj <- read.dt(fpath)

test_that("valid csdf object created", {
  expect_s4_class(obj, "csdf")
  expect_true(validObject(obj))
  expect_true(validObject.csdf(obj))
})
