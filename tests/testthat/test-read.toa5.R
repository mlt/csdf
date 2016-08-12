context("TOA5 reading")

Sys.setenv(TZ='GMT')
fpath <- system.file("extdata", "Station_Daily.dat", package="csdf")
obj <- read.toa5(fpath)

test_that("valid csdf object created", {
  expect_s4_class(obj, "csdf")
  expect_true(validObject(obj))
})
